#include "typecheck.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "identmap.h"
#include "io.h"
#include "parse.h"
#include "slice.h"
#include "table.h"
#include "util.h"

#define CHECK_DBG(...) do { } while (0)

struct import {
  ident_value import_name;
  struct ast_file *file;
};

void import_destroy(struct import *imp) {
  ast_file_destroy(imp->file);
  free(imp->file);
}

struct checkstate {
  module_loader *loader;
  struct ident_map *im;

  struct import *imports;
  size_t imports_count;
  size_t imports_limit;

  struct name_table nt;
};

void checkstate_init(struct checkstate *cs,
		     module_loader *loader,
		     struct ident_map *im) {
  cs->loader = loader;
  cs->im = im;
  cs->imports = NULL;
  cs->imports_count = 0;
  cs->imports_limit = 0;
  name_table_init(&cs->nt);
}

void intern_primitive_type(struct checkstate *cs,
			   const char *name,
			   int *flatly_held,
			   size_t flatly_held_count) {
  ident_value ident = ident_map_intern_c_str(cs->im, name);
  int res = name_table_add_primitive_type(&cs->nt, ident,
					  flatly_held, flatly_held_count);
  CHECK(res);
}

#define I32_TYPE_NAME "i32"
#define FUNC_TYPE_NAME "func"

void checkstate_import_primitives(struct checkstate *cs) {
  intern_primitive_type(cs, "u32", NULL, 0);
  intern_primitive_type(cs, I32_TYPE_NAME, NULL, 0);
  intern_primitive_type(cs, "f64", NULL, 0);
  int not_flatly_held[20] = { 0 };
  intern_primitive_type(cs, "ptr", not_flatly_held, 1);
  for (size_t i = 1; i < 21; i++) {
    intern_primitive_type(cs, "func", not_flatly_held, i);
  }
}

void checkstate_destroy(struct checkstate *cs) {
  name_table_destroy(&cs->nt);
  SLICE_FREE(cs->imports, cs->imports_count, import_destroy);
  cs->imports_limit = 0;
  cs->im = NULL;
}

int resolve_import_filename_and_parse(struct checkstate *cs,
				      ident_value name,
				      struct ast_file *file_out) {
  int ret = 0;

  const void *module_name;
  size_t module_name_count;
  ident_map_lookup(cs->im, name, &module_name, &module_name_count);

  uint8_t *data;
  size_t data_size;
  if (!(*cs->loader)(module_name, module_name_count, &data, &data_size)) {
    ERR_DBG("Could not read file.\n");
    goto fail;
  }

  size_t error_pos;
  if (!parse_buf_file(cs->im, data, data_size, file_out, &error_pos)) {
    ERR_DBG("Could not parse import.\n");
    goto fail_data;
  }

  ret = 1;
 fail_data:
  free(data);
 fail:
  return ret;
}

int chase_imports(struct checkstate *cs, ident_value name) {
  int ret = 0;
  ident_value *names = NULL;
  size_t names_count = 0;
  size_t names_limit = 0;

  SLICE_PUSH(names, names_count, names_limit, name);

  while (names_count) {
    name = names[--names_count];

    for (size_t i = 0, e = cs->imports_count; i < e; i++) {
      if (cs->imports[i].import_name == name) {
	goto continue_outer;
      }
    }

    struct ast_file file;
    if (!resolve_import_filename_and_parse(cs, name, &file)) {
      goto cleanup;
    }

    struct ast_file *heap_file = malloc(sizeof(*heap_file));
    CHECK(heap_file);
    *heap_file = file;
    struct import imp;
    imp.import_name = name;
    imp.file = heap_file;
    SLICE_PUSH(cs->imports, cs->imports_count, cs->imports_limit, imp);

    for (size_t i = 0, e = heap_file->toplevels_count; i < e; i++) {
      struct ast_toplevel *toplevel = &heap_file->toplevels[i];
      switch (toplevel->tag) {
      case AST_TOPLEVEL_IMPORT:
	SLICE_PUSH(names, names_count, names_limit, toplevel->u.import.name.value);
	break;
      case AST_TOPLEVEL_DEF: {
	if (!name_table_add_def(&cs->nt,
				toplevel->u.def.name.value,
				&toplevel->u.def.generics,
				&toplevel->u.def.type,
				&toplevel->u.def)) {
	  goto cleanup;
	}
      } break;
      case AST_TOPLEVEL_DEFTYPE: {
	if (!name_table_add_deftype(&cs->nt,
				    toplevel->u.deftype.name.value,
				    params_arity(&toplevel->u.deftype.generics),
				    &toplevel->u.deftype)) {
	  goto cleanup;
	}
      } break;
      default:
	UNREACHABLE();
      }
    }

  continue_outer:
    continue;
  }

  ret = 1;
 cleanup:
  free(names);
  return ret;
}

int lookup_import(struct checkstate *cs, ident_value name,
		  struct ast_file **file_out) {
  for (size_t i = 0, e = cs->imports_count; i < e; i++) {
    if (cs->imports[i].import_name == name) {
      *file_out = cs->imports[i].file;
      return 1;
    }
  }
  return 0;
}

int generics_lookup_name(struct ast_generics *a,
			 ident_value name,
			 size_t *index_out) {
  if (!a->has_type_params) {
    return 0;
  }
  for (size_t i = 0, e = a->params_count; i < e; i++) {
    if (a->params[i].value == name) {
      *index_out = i;
      return 1;
    }
  }
  return 0;
}

int check_deftype(struct checkstate *cs, struct deftype_entry *ent);
int check_typeexpr(struct checkstate *cs,
		   struct ast_generics *generics,
		   struct ast_typeexpr *a,
		   struct deftype_entry *flat_typeexpr);

int check_typeexpr_name(struct checkstate *cs,
			struct ast_generics *generics,
			struct ast_ident *a,
			struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_typeexpr_name\n");
  ident_value name = a->value;
  size_t which_generic;
  if (generics_lookup_name(generics, name, &which_generic)) {
    if (flat_typeexpr) {
      deftype_entry_mark_generic_flatly_held(flat_typeexpr,
					     which_generic);
    }
  } else {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, name, no_param_list_arity(),
				   &ent)) {
      ERR_DBG("Unrecognized type name.\n");
      return 0;
    }

    if (flat_typeexpr) {
      if (!check_deftype(cs, ent)) {
	return 0;
      }
    }
  }

  return 1;
}

int check_typeexpr_app(struct checkstate *cs,
		       struct ast_generics *generics,
		       struct ast_typeapp *a,
		       struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_typeexpr_app\n");
  struct deftype_entry *ent;
  if (!name_table_lookup_deftype(&cs->nt, a->name.value,
				 param_list_arity(a->params_count),
				 &ent)) {
    ERR_DBG("Unrecognized type name/bad arity.\n");
    return 0;
  }

  if (flat_typeexpr) {
    if (!check_deftype(cs, ent)) {
      return 0;
    }
  }

  for (size_t i = 0, e = a->params_count; i < e; i++) {
    int f = deftype_entry_param_is_flatly_held(ent, i);
    if (!check_typeexpr(cs, generics, &a->params[i], f ? flat_typeexpr : NULL)) {
      return 0;
    }
  }

  return 1;
}

int check_typeexpr_fields(struct checkstate *cs,
			  struct ast_generics *generics,
			  struct ast_vardecl *fields,
			  size_t fields_count,
			  struct deftype_entry *flat_typeexpr) {
  for (size_t i = 0; i < fields_count; i++) {
    struct ast_vardecl *field = &fields[i];
    for (size_t j = 0; j < i; j++) {
      if (field->name.value == fields[j].name.value) {
	ERR_DBG("struct/union fields have duplicate name.\n");
	return 0;
      }
    }

    {
      size_t which_generic;
      if (generics_lookup_name(generics, field->name.value, &which_generic)) {
	ERR_DBG("struct/union field shadows template parameter, which is gauche.\n");
	return 0;
      }
    }

    if (!check_typeexpr(cs, generics, &field->type, flat_typeexpr)) {
      return 0;
    }
  }

  return 1;
}

int check_typeexpr(struct checkstate *cs,
		   struct ast_generics *generics,
		   struct ast_typeexpr *a,
		   struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_typeexpr\n");
  /* null means we have to worry about flatness, non-null means we don't. */
  switch (a->tag) {
  case AST_TYPEEXPR_NAME:
    return check_typeexpr_name(cs, generics, &a->u.name, flat_typeexpr);
  case AST_TYPEEXPR_APP:
    return check_typeexpr_app(cs, generics, &a->u.app, flat_typeexpr);
  case AST_TYPEEXPR_STRUCTE:
    return check_typeexpr_fields(cs, generics,
				 a->u.structe.fields,
				 a->u.structe.fields_count,
				 flat_typeexpr);
  case AST_TYPEEXPR_UNIONE:
    return check_typeexpr_fields(cs, generics,
				 a->u.structe.fields,
				 a->u.structe.fields_count,
				 flat_typeexpr);
  default:
    UNREACHABLE();
  }
}

int check_generics_shadowing(struct checkstate *cs,
			     struct ast_generics *a) {
  if (!a->has_type_params) {
    return 1;
  }

  for (size_t i = 0, e = a->params_count; i < e; i++) {
    ident_value name = a->params[i].value;
    for (size_t j = 0; j < i; j++) {
      if (name == a->params[j].value) {
	ERR_DBG("duplicate param names within same generics list.\n");
	return 0;
      }
    }

    if (name_table_shadowed(&cs->nt, name)) {
      ERR_DBG("generics list shadows global name.\n");
      return 0;
    }
  }

  return 1;
}

int check_deftype(struct checkstate *cs, struct deftype_entry *ent) {
  CHECK_DBG("check_deftype\n");
  if (ent->has_been_checked) {
    return 1;
  }

  /* Can't be primitive, because they all have has_been_checked be true. */
  CHECK(!ent->is_primitive);

  if (ent->is_being_checked) {
    ERR_DBG("deftype recursively held.\n");
    return 0;
  }

  deftype_entry_mark_is_being_checked(ent);

  struct ast_deftype *a = ent->deftype;
  CHECK(a);  /* Must be non-null, because ent->is_primitive is false. */

  /* We know there's no clashes with a->name and the _arity_ of a->generics. */
  if (!check_generics_shadowing(cs, &a->generics)) {
    return 0;
  }

  if (!check_typeexpr(cs, &a->generics, &a->type, ent)) {
    return 0;
  }

  deftype_entry_mark_has_been_checked(ent);
  return 1;
}

struct exprscope {
  struct checkstate *cs;
  struct ast_generics *generics;
  struct ast_typeexpr *generics_substitutions;
  /* 0 if !generics->has_type_params; otherwise, equal to
     generics->params_count. */
  size_t generics_substitutions_count;

  /* A stack of variables that are in scope. */
  struct ast_vardecl **vars;
  size_t vars_count;
  size_t vars_limit;
};

void exprscope_init(struct exprscope *es, struct checkstate *cs,
		    struct ast_generics *generics,
		    struct ast_typeexpr *generics_substitutions,
		    size_t generics_substitutions_count) {
  CHECK(generics->params_count == (generics->has_type_params ?
				   generics_substitutions_count : 0));
  es->cs = cs;
  es->generics = generics;
  es->generics_substitutions = generics_substitutions;
  es->generics_substitutions_count = generics_substitutions_count;
  es->vars = NULL;
  es->vars_count = 0;
  es->vars_limit = 0;
}

void exprscope_destroy(struct exprscope *es) {
  es->cs = NULL;
  es->generics = NULL;
  SLICE_FREE(es->generics_substitutions, es->generics_substitutions_count,
	     ast_typeexpr_destroy);
  free(es->vars);
  es->vars = NULL;
  es->vars_count = 0;
  es->vars_limit = 0;
}

int unify_fields_directionally(struct ast_vardecl *partial_fields,
			       size_t partial_fields_count,
			       struct ast_vardecl *complete_fields,
			       size_t complete_fields_count) {
  if (partial_fields_count != complete_fields_count) {
    return 0;
  }

  for (size_t i = 0; i < partial_fields_count; i++) {
    if (partial_fields[i].name.value != complete_fields[i].name.value
	|| !unify_directionally(&partial_fields[i].type,
				&complete_fields[i].type)) {
      return 0;
    }
  }

  return 1;
}

int unify_directionally(struct ast_typeexpr *partial_type,
			struct ast_typeexpr *complete_type) {
  CHECK(complete_type->tag != AST_TYPEEXPR_UNKNOWN);
  if (partial_type->tag == AST_TYPEEXPR_UNKNOWN) {
    return 1;
  }

  if (partial_type->tag != complete_type->tag) {
    return 0;
  }

  switch (partial_type->tag) {
  case AST_TYPEEXPR_NAME: {
    return partial_type->u.name.value == complete_type->u.name.value;
  } break;
  case AST_TYPEEXPR_APP: {
    struct ast_typeapp *p_app = &partial_type->u.app;
    struct ast_typeapp *c_app = &complete_type->u.app;
    if (p_app->name.value != c_app->name.value
	|| p_app->params_count != c_app->params_count) {
      return 0;
    }
    for (size_t i = 0, e = p_app->params_count; i < e; i++) {
      if (!unify_directionally(&p_app->params[i], &c_app->params[i])) {
	return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return unify_fields_directionally(partial_type->u.structe.fields,
				      partial_type->u.structe.fields_count,
				      complete_type->u.structe.fields,
				      complete_type->u.structe.fields_count);
  case AST_TYPEEXPR_UNIONE:
    return unify_fields_directionally(partial_type->u.unione.fields,
				      partial_type->u.unione.fields_count,
				      complete_type->u.unione.fields,
				      complete_type->u.unione.fields_count);
  default:
    UNREACHABLE();
  }
}

int exprscope_lookup_name(struct exprscope *es,
			  ident_value name,
			  struct ast_typeexpr *partial_type,
			  struct ast_typeexpr *out) {
  for (size_t i = es->vars_count; i-- > 0; ) {
    struct ast_vardecl *decl = es->vars[i];
    if (decl->name.value != name) {
      continue;
    }

    if (!unify_directionally(partial_type, &decl->type)) {
      ERR_DBG("Type mismatch for vardecl lookup.\n");
      return 0;
    }

    ast_typeexpr_init_copy(out, &decl->type);
    return 1;
  }

  struct ast_typeexpr unified;
  struct def_entry *ent;
  if (name_table_match_def(&es->cs->nt,
			   name,
			   NULL, /* No generic typeexpr parameters on this expr */
			   0,
			   partial_type,
			   &unified, &ent)) {
    *out = unified;
    return 1;
  }

  ERR_DBG("Could not match def for name.\n");
  return 0;
}

void numeric_literal_type(struct ident_map *im,
			  struct ast_numeric_literal *a,
			  struct ast_typeexpr *out) {
  (void)a;
  out->tag = AST_TYPEEXPR_NAME;
  ast_ident_init(&out->u.name, ast_meta_make_garbage(),
		 ident_map_intern_c_str(im, I32_TYPE_NAME));
}

void do_replace_generics(struct ast_generics *generics,
			 struct ast_typeexpr *generics_substitutions,
			 struct ast_typeexpr *a,
			 struct ast_typeexpr *out);

void do_replace_generics_in_fields(struct ast_generics *generics,
				   struct ast_typeexpr *generics_substitutions,
				   struct ast_vardecl *fields,
				   size_t fields_count,
				   struct ast_vardecl **fields_out,
				   size_t *fields_count_out) {
  struct ast_vardecl *f = malloc(size_mul(sizeof(*f), fields_count));
  CHECK(f || fields_count == 0);
  for (size_t i = 0; i < fields_count; i++) {
    struct ast_ident name;
    ast_ident_init_copy(&name, &fields[i].name);
    struct ast_typeexpr type;
    do_replace_generics(generics, generics_substitutions, &fields[i].type,
			&type);
    ast_vardecl_init(&f[i], ast_meta_make_garbage(),
		     name, type);
  }
  *fields_out = f;
  *fields_count_out = fields_count;
}

void do_replace_generics(struct ast_generics *generics,
			 struct ast_typeexpr *generics_substitutions,
			 struct ast_typeexpr *a,
			 struct ast_typeexpr *out) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME: {
    size_t which_generic;
    if (generics_lookup_name(generics, a->u.name.value, &which_generic)) {
      ast_typeexpr_init_copy(out, &generics_substitutions[which_generic]);
    } else {
      ast_typeexpr_init_copy(out, a);
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct ast_typeapp *app = &a->u.app;
    size_t params_count = app->params_count;
    struct ast_typeexpr *params = malloc(size_mul(sizeof(*params), app->params_count));
    CHECK(params || params_count == 0);

    for (size_t i = 0, e = params_count; i < e; i++) {
      do_replace_generics(generics, generics_substitutions,
			  &app->params[i], &params[i]);
    }

    struct ast_ident name;
    ast_ident_init_copy(&name, &app->name);

    out->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&out->u.app, ast_meta_make_garbage(),
		     name, params, params_count);
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    struct ast_vardecl *fields;
    size_t fields_count;
    do_replace_generics_in_fields(generics, generics_substitutions,
				  a->u.structe.fields, a->u.structe.fields_count,
				  &fields, &fields_count);
    out->tag = AST_TYPEEXPR_STRUCTE;
    ast_structe_init(&out->u.structe, ast_meta_make_garbage(),
		     fields, fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    struct ast_vardecl *fields;
    size_t fields_count;
    do_replace_generics_in_fields(generics, generics_substitutions,
				  a->u.unione.fields, a->u.unione.fields_count,
				  &fields, &fields_count);
    out->tag = AST_TYPEEXPR_UNIONE;
    ast_unione_init(&out->u.unione, ast_meta_make_garbage(),
		     fields, fields_count);
  } break;
  default:
    UNREACHABLE();
  }
}

void replace_generics(struct exprscope *es,
		      struct ast_typeexpr *a,
		      struct ast_typeexpr *out) {
  if (!es->generics->has_type_params) {
    ast_typeexpr_init_copy(out, a);
  } else {
    CHECK(es->generics->params_count == es->generics_substitutions_count);
    do_replace_generics(es->generics, es->generics_substitutions, a, out);
  }
}

int check_expr(struct exprscope *es,
	       struct ast_expr *x,
	       struct ast_typeexpr *partial_type,
	       struct ast_typeexpr *out);

int check_expr_funcall(struct exprscope *es,
		       struct ast_funcall *x,
		       struct ast_typeexpr *partial_type,
		       struct ast_typeexpr *out) {
  /* TODO: We also need to type-check any template instantiations. */

  size_t args_count = x->args_count;
  size_t args_types_count = size_add(args_count, 1);
  struct ast_typeexpr *args_types = malloc(size_mul(sizeof(*args_types),
						    args_types_count));
  CHECK(args_types);
  size_t i;
  for (i = 0; i < args_count; i++) {
    struct ast_typeexpr local_partial;
    local_partial.tag = AST_TYPEEXPR_UNKNOWN;
    if (!check_expr(es, &x->args[i], &local_partial, &args_types[i])) {
      goto fail_cleanup_args_types;
    }
  }

  ast_typeexpr_init_copy(&args_types[args_count], partial_type);

  ident_value func_ident = ident_map_intern_c_str(es->cs->im, FUNC_TYPE_NAME);
  struct ast_ident name;
  ast_ident_init(&name, ast_meta_make_garbage(), func_ident);

  struct ast_typeexpr funcexpr;
  funcexpr.tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(), name, args_types, args_types_count);

  int ret = 0;
  struct ast_typeexpr resolved_funcexpr;
  if (!check_expr(es, x->func, &funcexpr, &resolved_funcexpr)) {
    goto fail_cleanup_funcexpr;
  }

  CHECK(resolved_funcexpr.tag == AST_TYPEEXPR_APP);
  CHECK(resolved_funcexpr.u.app.name.value == func_ident);
  CHECK(resolved_funcexpr.u.app.params_count == args_types_count);
  ast_typeexpr_init_copy(out, &resolved_funcexpr.u.app.params[args_count]);

  ret = 1;
 fail_cleanup_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  return ret;
  /* Don't fall-through -- args_types was moved into funcexpr. */
 fail_cleanup_args_types:
  SLICE_FREE(args_types, i, ast_typeexpr_destroy);
  return 0;
}

int check_var_shadowing(struct exprscope *es, ident_value name) {
  for (size_t i = 0, e = es->vars_count; i < e; i++) {
    if (es->vars[i]->name.value == name) {
      ERR_DBG("Variable name shadows local.\n");
      return 0;
    }
  }

  {
    size_t which_generic;
    if (generics_lookup_name(es->generics, name, &which_generic)) {
      ERR_DBG("Variable name shadows template parameter, which is gauche.\n");
      return 0;
    }
  }

  if (name_table_shadowed(&es->cs->nt, name)) {
    ERR_DBG("Variable name shadows a global def or type.\n");
    return 0;
  }

  return 1;
}

int check_expr_lambda(struct exprscope *es,
		      struct ast_lambda *x,
		      struct ast_typeexpr *partial_type,
		      struct ast_typeexpr *out) {
  ident_value func_ident = ident_map_intern_c_str(es->cs->im, FUNC_TYPE_NAME);
  size_t func_params_count = x->params_count;
  size_t args_count = size_add(func_params_count, 1);

  struct ast_typeexpr funcexpr;
  {
    struct ast_typeexpr *args = malloc(size_mul(sizeof(*args), args_count));
    CHECK(args);
    size_t i;
    for (i = 0; i < func_params_count; i++) {
      for (size_t j = 0; j < i; j++) {
	if (x->params[i].name.value == x->params[j].name.value) {
	  ERR_DBG("Duplicate lambda parameter name.\n");
	  goto fail_args_up_to_i;
	}
      }
      if (!check_var_shadowing(es, x->params[i].name.value)) {
	goto fail_args_up_to_i;
      }

      if (!check_typeexpr(es->cs, es->generics, &x->params[i].type, NULL)) {
	ERR_DBG("Invalid type.\n");
	goto fail_args_up_to_i;
      }

      replace_generics(es, &x->params[i].type, &args[i]);
    }

    if (0) {
    fail_args_up_to_i:
      SLICE_FREE(args, i, ast_typeexpr_destroy);
      return 0;
    }

    ast_typeexpr_init_copy(&args[func_params_count], &x->return_type);

    struct ast_ident name;
    ast_ident_init(&name, ast_meta_make_garbage(), func_ident);

    funcexpr.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
		     name, args, args_count);
  }

  if (!unify_directionally(partial_type, &funcexpr)) {
    goto fail_funcexpr;
  }

  /* TODO: Typecheck bracebody. */

  *out = funcexpr;
  return 1;

 fail_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  return 0;
}

int check_expr(struct exprscope *es,
	       struct ast_expr *x,
	       struct ast_typeexpr *partial_type,
	       struct ast_typeexpr *out) {
  switch (x->tag) {
  case AST_EXPR_NAME: {
    struct ast_typeexpr name_type;
    if (!exprscope_lookup_name(es, x->u.name.value, partial_type, &name_type)) {
      ERR_DBG("Unrecognized name.\n");
      return 0;
    }

    *out = name_type;
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    struct ast_typeexpr num_type;
    numeric_literal_type(es->cs->im, &x->u.numeric_literal, &num_type);
    if (!unify_directionally(partial_type, &num_type)) {
      ERR_DBG("Numeric literal in bad place.\n");
      ast_typeexpr_destroy(&num_type);
      return 0;
    }

    *out = num_type;
    return 1;
  } break;
  case AST_EXPR_FUNCALL: {
    return check_expr_funcall(es, &x->u.funcall, partial_type, out);
  } break;
  case AST_EXPR_UNOP:
  case AST_EXPR_BINOP:
    /* TODO: Implement. */
    return 0;
  case AST_EXPR_LAMBDA: {
    return check_expr_lambda(es, &x->u.lambda, partial_type, out);
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    /* TODO: Implement. */
    return 0;
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    /* TODO: Implement. */
    return 0;
  } break;
  default:
    UNREACHABLE();
  }
}

/* Checks an expr, given that we know the type of expr. */
int check_expr_with_type(struct exprscope *es,
			 struct ast_expr *x,
			 struct ast_typeexpr *type) {
  struct ast_typeexpr out;
  int ret = check_expr(es, x, type, &out);
  if (ret) {
    /* TODO: Assert that the types are identical? */
    ast_typeexpr_destroy(&out);
  }
  return ret;
}

int check_def(struct checkstate *cs, struct ast_def *a) {
  if (!check_generics_shadowing(cs, &a->generics)) {
    return 0;
  }

  if (!check_typeexpr(cs, &a->generics, &a->type, NULL)) {
    return 0;
  }

  /* We can only typecheck the def by instantiating it -- so we check
     the ones with no template params. */
  if (!a->generics.has_type_params) {
    struct exprscope es;
    exprscope_init(&es, cs, &a->generics, NULL, 0);
    int ret = check_expr_with_type(&es, &a->rhs, &a->type);
    exprscope_destroy(&es);
    return ret;
  } else {
    return 1;
  }
}

int check_toplevel(struct checkstate *cs, struct ast_toplevel *a) {
  (void)cs;
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    /* We already parsed and loaded the import. */
    return 1;
  case AST_TOPLEVEL_DEF:
    return check_def(cs, &a->u.def);
  case AST_TOPLEVEL_DEFTYPE:
    return check_deftype(cs, lookup_deftype(&cs->nt, &a->u.deftype));
  default:
    UNREACHABLE();
  }
}

int check_module(struct ident_map *im, module_loader *loader, ident_value name) {
  int ret = 0;
  struct checkstate cs;
  checkstate_init(&cs, loader, im);
  checkstate_import_primitives(&cs);

  if (!chase_imports(&cs, name)) {
    goto cleanup;
  }

  struct ast_file *file;
  if (!lookup_import(&cs, name, &file)) {
    CRASH("lookup_import just failed after chase_imports succeeded.\n");
  }

  for (size_t i = 0, e = file->toplevels_count; i < e; i++) {
    if (!check_toplevel(&cs, &file->toplevels[i])) {
      goto cleanup;
    }
  }

  ret = 1;

 cleanup:
  checkstate_destroy(&cs);
  return ret;
}

int read_module_file(const uint8_t *module_name,
		     size_t module_name_count,
		     uint8_t **data_out,
		     size_t *data_size_out) {
  int ret = 0;
  char *filename;
  {
    filename = malloc(size_add(module_name_count, 4));
    CHECK(filename);
    memcpy(filename, module_name, module_name_count);
    memcpy(filename + module_name_count, ".ki", 4);
  }

  if (!read_file(filename, data_out, data_size_out)) {
    ERR_DBG("Could not read file.\n");
  } else {
    ret = 1;
  }

  free(filename);
  return ret;
}

struct test_module {
  const char *name;
  const char *data;
};

int load_test_module(struct test_module *a, size_t a_count,
		     const uint8_t *name, size_t name_count,
		     uint8_t **data_out, size_t *data_count_out) {
  for (size_t i = 0; i < a_count; i++) {
    if (strlen(a[i].name) == name_count
	&& 0 == memcmp(a[i].name, name, name_count)) {
      STATIC_CHECK(sizeof(uint8_t) == 1);
      size_t data_count = strlen(a[i].data);
      uint8_t *data = malloc(data_count);
      CHECK(data || data_count == 0);
      memcpy(data, a[i].data, data_count);
      *data_out = data;
      *data_count_out = data_count;
      return 1;
    }
  }
  return 0;
}

int check_file_test_1(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "import bar;\n"
			       "\n"
			       "def x i32 = 3;"
			       "deftype dword u32;\n" },
			     { "bar",
			       "import foo;\n"
			       "\n"
			       "def y f64 = 5;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}


int check_file_test_2(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;"
			       "deftype dword u32;\n"
			       "deftype blah dword;\n"
			       "deftype feh ptr[blah];\n"
			       "deftype quux ptr[quux];\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_3(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  /* An invalid file: bar and foo recursively hold each other. */
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;"
			       "deftype foo bar;\n"
			       "deftype bar foo;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_4(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;"
			       "deftype foo struct { x u32; y f64; z ptr[foo]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_5(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;"
			       "deftype[T] foo T;" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_6(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;"
			       "deftype[T] foo struct { count u32; p ptr[T]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_7(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  /* This fails because bar recursively holds itself through a
     template parameter. */
  struct test_module a[] = { { "foo",
			       "deftype[T, U] foo struct { x ptr[T]; y U; };\n"
			       "deftype bar struct { z foo[u32, bar]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_8(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  /* But here bar holds itself indirectly. */
  struct test_module a[] = { { "foo",
			       "deftype[T, U] foo struct { x ptr[T]; y U; };\n"
			       "deftype bar struct { z foo[bar, u32]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_def_1(const uint8_t *name, size_t name_count,
			  uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_def_2(const uint8_t *name, size_t name_count,
			  uint8_t **data_out, size_t *data_count_out) {
  /* Fails because numeric literals are dumb and have type i32. */
  struct test_module a[] = { { "foo",
			       "def x u32 = 3;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int check_file_test_def_3(const uint8_t *name, size_t name_count,
			  uint8_t **data_out, size_t *data_count_out) {
  /* Fails because numeric literals are dumb and have type i32. */
  struct test_module a[] = { { "foo",
			       "def x i32 = 3;\n"
			       "def y i32 = x;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}


int test_check_file(void) {
  int ret = 0;
  struct ident_map im;
  ident_map_init(&im);
  ident_value foo = ident_map_intern_c_str(&im, "foo");

  DBG("test_check_file check_file_test_1...\n");
  if (!check_module(&im, &check_file_test_1, foo)) {
    DBG("check_file_test_1 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_2...\n");
  if (!check_module(&im, &check_file_test_2, foo)) {
    DBG("check_file_test_2 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file !check_file_test_3...\n");
  if (!!check_module(&im, &check_file_test_3, foo)) {
    DBG("!check_file_test_3 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_4...\n");
  if (!check_module(&im, &check_file_test_4, foo)) {
    DBG("check_file_test_4 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_5...\n");
  if (!check_module(&im, &check_file_test_5, foo)) {
    DBG("check_file_test_5 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_6...\n");
  if (!check_module(&im, &check_file_test_6, foo)) {
    DBG("check_file_test_6 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file !check_file_test_7...\n");
  if (!!check_module(&im, &check_file_test_7, foo)) {
    DBG("!check_file_test_7 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_8...\n");
  if (!check_module(&im, &check_file_test_8, foo)) {
    DBG("check_file_test_8 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_def_1...\n");
  if (!check_module(&im, &check_file_test_def_1, foo)) {
    DBG("check_file_test_def_1 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file !check_file_test_def_2...\n");
  if (!!check_module(&im, &check_file_test_def_2, foo)) {
    DBG("check_file_test_def_2 fails\n");
    goto cleanup_ident_map;
  }

  DBG("test_check_file check_file_test_def_3...\n");
  if (!check_module(&im, &check_file_test_def_3, foo)) {
    DBG("check_file_test_def_3 fails\n");
    goto cleanup_ident_map;
  }

  ret = 1;
 cleanup_ident_map:
  ident_map_destroy(&im);
  return ret;
}
