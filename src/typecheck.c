#include "typecheck.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "checkstate.h"
#include "identmap.h"
#include "io.h"
#include "parse.h"
#include "slice.h"
#include "table.h"
#include "util.h"

#define CHECK_DBG(...) do { } while (0)

const int MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH = 50;

struct ast_ident make_ast_ident(ident_value ident) {
  struct ast_ident ret;
  ast_ident_init(&ret, ast_meta_make_garbage(), ident);
  return ret;
}

void intern_primitive_type(struct checkstate *cs,
                           const char *name,
                           int *flatly_held,
                           size_t flatly_held_count,
                           uint32_t primitive_sizeof,
                           uint32_t primitive_alignof) {
  ident_value ident = identmap_intern_c_str(cs->im, name);
  int res = name_table_add_primitive_type(&cs->nt, ident,
                                          flatly_held, flatly_held_count,
                                          primitive_sizeof,
                                          primitive_alignof);
  CHECK(res);
}

int is_magic_unop(enum ast_unop unop) {
  return unop == AST_UNOP_DEREFERENCE || unop == AST_UNOP_ADDRESSOF;
}

const char *unop_fakename(enum ast_unop unop) {
  /* First check that unop isn't some magic lvalue thing. */
  CHECK(!is_magic_unop(unop));

  static const char *const unop_names[] = {
    [AST_UNOP_DEREFERENCE] = NULL,
    [AST_UNOP_ADDRESSOF] = NULL,
    [AST_UNOP_NEGATE] = "$UNOP_NEGATE",
  };

  return unop_names[unop];
}

void intern_unop(struct checkstate *cs,
                 enum ast_unop unop,
                 struct ast_generics *generics,
                 struct ast_typeexpr *type) {
  name_table_add_primitive_def(
      &cs->nt,
      identmap_intern_c_str(cs->im, unop_fakename(unop)),
      generics,
      type);
}

int is_magic_binop(enum ast_binop binop) {
  return binop == AST_BINOP_ASSIGN || binop == AST_BINOP_LOGICAL_OR
    || binop == AST_BINOP_LOGICAL_AND;
}

const char *binop_fakename(enum ast_binop binop) {
  CHECK(!is_magic_binop(binop));

  static const char *const binop_names[] = {
    [AST_BINOP_ASSIGN] = NULL,
    [AST_BINOP_ADD] = "$BINOP_ADD",
    [AST_BINOP_SUB] = "$BINOP_SUB",
    [AST_BINOP_MUL] = "$BINOP_MUL",
    [AST_BINOP_DIV] = "$BINOP_DIV",
    [AST_BINOP_MOD] = "$BINOP_MOD",
    [AST_BINOP_LT] = "$BINOP_LT",
    [AST_BINOP_LE] = "$BINOP_LE",
    [AST_BINOP_GT] = "$BINOP_GT",
    [AST_BINOP_GE] = "$BINOP_GE",
    [AST_BINOP_EQ] = "$BINOP_EQ",
    [AST_BINOP_NE] = "$BINOP_NE",
    [AST_BINOP_BIT_XOR] = "$BINOP_BIT_XOR",
    [AST_BINOP_BIT_OR] = "$BINOP_BIT_OR",
    [AST_BINOP_BIT_AND] = "$BINOP_BIT_AND",
    [AST_BINOP_BIT_LEFTSHIFT] = "$BINOP_BIT_LEFTSHIFT",
    [AST_BINOP_BIT_RIGHTSHIFT] = "$BINOP_BIT_RIGHTSHIFT",
    [AST_BINOP_LOGICAL_OR] = NULL,
    [AST_BINOP_LOGICAL_AND] = NULL,
  };

  return binop_names[binop];
}

void intern_binop(struct checkstate *cs,
                  enum ast_binop binop,
                  struct ast_generics *generics,
                  struct ast_typeexpr *type) {

  name_table_add_primitive_def(&cs->nt,
                               identmap_intern_c_str(cs->im,
                                                     binop_fakename(binop)),
                               generics,
                               type);
}

#define CONVERT_FUNCTION_NAME "convert"

int typeexpr_is_func_type(struct identmap *im, struct ast_typeexpr *x) {
  return x->tag == AST_TYPEEXPR_APP
    && x->u.app.name.value == identmap_intern(im, FUNC_TYPE_NAME,
                                              strlen(FUNC_TYPE_NAME));
}

void checkstate_import_primitive_types(struct checkstate *cs) {
  intern_primitive_type(cs, VOID_TYPE_NAME, NULL, 0, 0, 1);
  intern_primitive_type(cs, BYTE_TYPE_NAME, NULL, 0, 1, 1);
  intern_primitive_type(cs, U32_TYPE_NAME, NULL, 0, 4, 4);
  intern_primitive_type(cs, I32_TYPE_NAME, NULL, 0, 4, 4);

  int not_flatly_held[20] = { 0 };
  /* X86 -- 32-bit pointers */
  intern_primitive_type(cs, PTR_TYPE_NAME, not_flatly_held, 1, 4, 4);
  for (size_t i = 1; i < 21; i++) {
    /* X86 -- 32-bit function pointers */
    intern_primitive_type(cs, FUNC_TYPE_NAME, not_flatly_held, i, 4, 4);
  }
}

void init_func_type(struct ast_typeexpr *a, struct identmap *im,
                    ident_value *args, size_t args_count) {
  a->tag = AST_TYPEEXPR_APP;
  struct ast_ident name
    = make_ast_ident(identmap_intern_c_str(im, FUNC_TYPE_NAME));
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), args_count);
  for (size_t i = 0; i < args_count; i++) {
    params[i].tag = AST_TYPEEXPR_NAME;
    params[i].u.name = make_ast_ident(args[i]);
  }
  ast_typeapp_init(&a->u.app, ast_meta_make_garbage(),
                   name, params, args_count);
}

void init_name_type(struct ast_typeexpr *a, ident_value name) {
  a->tag = AST_TYPEEXPR_NAME;
  a->u.name = make_ast_ident(name);
}

struct ast_typeexpr *expose_func_return_type(struct identmap *im,
                                             struct ast_typeexpr *func,
                                             size_t expected_params_count) {
  CHECK(func->tag == AST_TYPEEXPR_APP);
  CHECK(func->u.app.name.value == identmap_intern_c_str(im, FUNC_TYPE_NAME));
  CHECK(func->u.app.params_count == expected_params_count);
  return &func->u.app.params[expected_params_count - 1];
}

void copy_func_return_type(struct identmap *im,
                           struct ast_typeexpr *func,
                           size_t expected_params_count,
                           struct ast_typeexpr *out) {
  ast_typeexpr_init_copy(
      out, expose_func_return_type(im, func, expected_params_count));
}

void init_binop_func_type(struct ast_typeexpr *a, struct identmap *im,
                          const char *type_name) {
  ident_value name = identmap_intern_c_str(im, type_name);
  ident_value names[3];
  names[0] = names[1] = names[2] = name;
  init_func_type(a, im, names, 3);
}

void init_binop_compare_type(struct ast_typeexpr *a, struct identmap *im,
                             const char *type_name) {
  ident_value name = identmap_intern_c_str(im, type_name);
  ident_value bool_name = identmap_intern_c_str(im, BOOLEAN_STANDIN_TYPE_NAME);
  ident_value names[3];
  names[0] = names[1] = name;
  names[2] = bool_name;
  init_func_type(a, im, names, 3);
}

void import_integer_binops(struct checkstate *cs, const char *type_name) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  struct ast_typeexpr binop_type;
  init_binop_func_type(&binop_type, cs->im, type_name);
  for (enum ast_binop op = AST_BINOP_ADD; op < AST_BINOP_LT; op++) {
    intern_binop(cs, op, &generics, &binop_type);
  }
  for (enum ast_binop op = AST_BINOP_BIT_XOR;
       op < AST_BINOP_LOGICAL_OR;
       op++) {
    intern_binop(cs, op, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  init_binop_compare_type(&binop_type, cs->im, type_name);
  for (enum ast_binop op = AST_BINOP_LT; op < AST_BINOP_BIT_XOR; op++) {
    intern_binop(cs, op, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  ast_generics_destroy(&generics);
}

void import_integer_conversions(struct checkstate *cs) {
  ident_value types[3];
  types[0] = identmap_intern_c_str(cs->im, BYTE_TYPE_NAME);
  types[1] = identmap_intern_c_str(cs->im, I32_TYPE_NAME);
  types[2] = identmap_intern_c_str(cs->im, U32_TYPE_NAME);

  ident_value convert = identmap_intern_c_str(cs->im, CONVERT_FUNCTION_NAME);

  struct ast_generics generics;
  ast_generics_init_no_params(&generics);

  for (size_t i = 0; i < 3; i++) {
    for (size_t j = 0; j < 3; j++) {
      struct ast_typeexpr func_type;
      ident_value names[2];
      names[0] = types[i];
      names[1] = types[j];
      init_func_type(&func_type, cs->im, names, 2);
      name_table_add_primitive_def(&cs->nt,
                                   convert,
                                   &generics,
                                   &func_type);
      ast_typeexpr_destroy(&func_type);
    }
  }
}

void checkstate_import_primitive_defs(struct checkstate *cs) {
  import_integer_binops(cs, I32_TYPE_NAME);
  import_integer_binops(cs, U32_TYPE_NAME);
  import_integer_binops(cs, BYTE_TYPE_NAME);

  import_integer_conversions(cs);

  {
    struct ast_generics generics;
    ast_generics_init_no_params(&generics);

    /* Unary minus on i32. */
    {
      struct ast_typeexpr type;
      ident_value args[2];
      args[0] = args[1] = identmap_intern_c_str(cs->im, I32_TYPE_NAME);
      init_func_type(&type, cs->im, args, 2);
      intern_unop(cs, AST_UNOP_NEGATE, &generics, &type);
      ast_typeexpr_destroy(&type);
    }

    ast_generics_destroy(&generics);
  }
}

void checkstate_import_primitives(struct checkstate *cs) {
  checkstate_import_primitive_types(cs);
  checkstate_import_primitive_defs(cs);
}

void init_boolean_typeexpr(struct checkstate *cs, struct ast_typeexpr *a) {
  a->tag = AST_TYPEEXPR_NAME;
  a->u.name = make_ast_ident(identmap_intern_c_str(cs->im, I32_TYPE_NAME));
}

int resolve_import_filename_and_parse(struct checkstate *cs,
                                      module_loader *loader,
                                      ident_value name,
                                      struct ast_file *file_out) {
  int ret = 0;

  const void *module_name;
  size_t module_name_count;
  identmap_lookup(cs->im, name, &module_name, &module_name_count);

  uint8_t *data;
  size_t data_size;
  if (!(*loader)(module_name, module_name_count, &data, &data_size)) {
    ERR_DBG("Could not read file.\n");
    goto fail;
  }

  size_t error_pos;
  if (!parse_buf_file(cs->im, data, data_size, file_out, &error_pos)) {
    ERR_DBG("Could not parse file, at %"PRIz".\n", error_pos);
    goto fail_data;
  }

  ret = 1;
 fail_data:
  free(data);
 fail:
  return ret;
}

int chase_imports(struct checkstate *cs, module_loader *loader,
                  ident_value name) {
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
    if (!resolve_import_filename_and_parse(cs, loader, name, &file)) {
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
        SLICE_PUSH(names, names_count, names_limit,
                   toplevel->u.import.name.value);
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
      case AST_TOPLEVEL_EXTERN_DEF: {
        if (!name_table_add_extern_def(&cs->nt,
                                       toplevel->u.extern_def.name.value,
                                       &toplevel->u.extern_def.type)) {
          goto cleanup;
        }
      } break;
      case AST_TOPLEVEL_DEFTYPE: {
        if (!name_table_add_deftype(
                &cs->nt,
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
    if (!check_typeexpr(cs, generics, &a->params[i],
                        f ? flat_typeexpr : NULL)) {
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
        ERR_DBG("struct/union field shadows template parameter, "
                "which is gauche.\n");
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

enum static_computation {
  STATIC_COMPUTATION_NO,
  STATIC_COMPUTATION_YES,
};

struct exprscope {
  struct checkstate *cs;
  struct ast_generics *generics;
  struct ast_typeexpr *generics_substitutions;
  /* 0 if !generics->has_type_params; otherwise, equal to
     generics->params_count. */
  size_t generics_substitutions_count;

  /* "YES" if the expr must be statically computable. */
  enum static_computation computation;
  /* The def_entry for this expr, maybe.  We record static_referents
     that we see, if this is a statically-computed expression.  It
     would be null for expressions that aren't statically computed for
     a top-level def. */
  struct def_entry *entry_or_null;

  /* A stack of variables that are in scope. */
  struct ast_vardecl **vars;
  size_t vars_count;
  size_t vars_limit;
};

void exprscope_init(struct exprscope *es, struct checkstate *cs,
                    struct ast_generics *generics,
                    struct ast_typeexpr *generics_substitutions,
                    size_t generics_substitutions_count,
                    enum static_computation computation,
                    struct def_entry *entry_or_null) {
  CHECK(generics->params_count == (generics->has_type_params ?
                                   generics_substitutions_count : 0));
  es->cs = cs;
  es->generics = generics;
  es->generics_substitutions = generics_substitutions;
  es->generics_substitutions_count = generics_substitutions_count;
  es->computation = computation;
  es->entry_or_null = entry_or_null;
  es->vars = NULL;
  es->vars_count = 0;
  es->vars_limit = 0;
}

void exprscope_destroy(struct exprscope *es) {
  es->cs = NULL;
  es->generics = NULL;
  es->generics_substitutions = NULL;
  es->generics_substitutions_count = 0;
  es->computation = STATIC_COMPUTATION_YES;
  free(es->vars);
  es->vars = NULL;
  es->vars_count = 0;
  es->vars_limit = 0;
}

void exprscope_note_static_reference(struct exprscope *es,
                                     struct def_entry *ent) {
  if (es->computation == STATIC_COMPUTATION_YES && es->entry_or_null) {
    def_entry_note_static_reference(es->entry_or_null, ent);
  }
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

int exprscope_push_var(struct exprscope *es, struct ast_vardecl *var) {
  if (!check_var_shadowing(es, var->name.value)) {
    ERR_DBG("Var name shadows.\n");
    return 0;
  }
  SLICE_PUSH(es->vars, es->vars_count, es->vars_limit, var);
  return 1;
}

void exprscope_pop_var(struct exprscope *es) {
  CHECK(es->vars_count > 0);
  --es->vars_count;
  es->vars[es->vars_count] = NULL;
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

int exact_typeexprs_equal(struct ast_typeexpr *a, struct ast_typeexpr *b) {
  return unify_directionally(a, b) && unify_directionally(b, a);
}

int check_expr_with_type(struct exprscope *es,
                         struct ast_expr *x,
                         struct ast_typeexpr *type,
                         struct ast_expr *annotated_out);

int lookup_global_maybe_typecheck(struct exprscope *es,
                                  ident_value name,
                                  struct ast_typeexpr *partial_type,
                                  struct ast_typeexpr *out,
                                  int *is_lvalue_out,
                                  struct def_instantiation **inst_out) {
  struct ast_typeexpr unified;
  struct def_entry *ent;
  struct def_instantiation *inst;
  if (!name_table_match_def(&es->cs->nt,
                            name,
                            NULL, /* No generic params in the expr here. */
                            0,
                            partial_type,
                            &unified,
                            &ent,
                            &inst)) {
    return 0;
  }

  exprscope_note_static_reference(es, ent);

  if (!ent->is_primitive && !ent->is_extern
      && ent->generics.has_type_params && !inst->typecheck_started) {
    CHECK(!inst->annotated_rhs_computed);
    if (es->cs->template_instantiation_recursion_depth
        == MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH) {
      ERR_DBG("Max template instantiation recursion depth exceeded.\n");
      goto fail_unified;
    }

    es->cs->template_instantiation_recursion_depth++;

    inst->typecheck_started = 1;
    struct exprscope scope;
    exprscope_init(&scope, es->cs,
                   &ent->def->generics,
                   inst->substitutions,
                   inst->substitutions_count,
                   STATIC_COMPUTATION_YES,
                   ent);

    struct ast_expr annotated_rhs;
    if (!check_expr_with_type(&scope, &ent->def->rhs, &unified,
                              &annotated_rhs)) {
      exprscope_destroy(&scope);
      es->cs->template_instantiation_recursion_depth--;
      goto fail_unified;
    }
    CHECK(!inst->annotated_rhs_computed);
    inst->annotated_rhs_computed = 1;
    inst->annotated_rhs = annotated_rhs;

    exprscope_destroy(&scope);
    es->cs->template_instantiation_recursion_depth--;
  }

  *out = unified;
  /* Right now, there are no global variables. */
  *is_lvalue_out = 0;
  *inst_out = inst;
  return 1;
 fail_unified:
  ast_typeexpr_destroy(&unified);
  return 0;
}

int exprscope_lookup_name(struct exprscope *es,
                          ident_value name,
                          struct ast_typeexpr *partial_type,
                          struct ast_typeexpr *out,
                          int *is_lvalue_out,
                          struct def_instantiation **inst_or_null_out) {
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
    *is_lvalue_out = 1;
    *inst_or_null_out = NULL;  /* NULL because it's a local. */
    return 1;
  }

  /* inst_or_null_out gets initialized to a non-NULL value. */
  return lookup_global_maybe_typecheck(es, name, partial_type,
                                       out, is_lvalue_out, inst_or_null_out);
}

void numeric_literal_type(struct identmap *im,
                          struct ast_numeric_literal *a,
                          struct ast_typeexpr *out) {
  out->tag = AST_TYPEEXPR_NAME;
  const char *type_name;
  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED:
    type_name = I32_TYPE_NAME;
    break;
  case AST_NUMERIC_TYPE_UNSIGNED:
    type_name = U32_TYPE_NAME;
    break;
  default:
    UNREACHABLE();
  }
  out->u.name = make_ast_ident(identmap_intern_c_str(im, type_name));
}

void do_replace_generics_in_fields(struct ast_generics *generics,
                                   struct ast_typeexpr *generics_substitutions,
                                   struct ast_vardecl *fields,
                                   size_t fields_count,
                                   struct ast_vardecl **fields_out,
                                   size_t *fields_count_out) {
  struct ast_vardecl *f = malloc_mul(sizeof(*f), fields_count);
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
    struct ast_typeexpr *params = malloc_mul(sizeof(*params),
                                             params_count);

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
                                  a->u.structe.fields,
                                  a->u.structe.fields_count,
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
               struct ast_typeexpr *out,
               int *is_lvalue_out,
               struct ast_expr *annotated_out);

int check_expr_funcall(struct exprscope *es,
                       struct ast_funcall *x,
                       struct ast_typeexpr *partial_type,
                       struct ast_typeexpr *out,
                       struct ast_funcall *annotated_out) {
  if (es->computation == STATIC_COMPUTATION_YES) {
    /* For now, there are no functions that are statically
       callable. */
    ERR_DBG("Function call in static expression.\n");
    return 0;
  }
  size_t args_count = x->args_count;
  size_t args_types_count = size_add(args_count, 1);
  struct ast_typeexpr *args_types = malloc_mul(sizeof(*args_types),
                                               args_types_count);
  struct ast_expr *args_annotated = malloc_mul(sizeof(*args_annotated),
                                               args_count);
  size_t i;
  for (i = 0; i < args_count; i++) {
    struct ast_typeexpr local_partial;
    local_partial.tag = AST_TYPEEXPR_UNKNOWN;
    int lvalue_discard;
    if (!check_expr(es, &x->args[i], &local_partial,
                    &args_types[i], &lvalue_discard,
                    &args_annotated[i])) {
      goto fail_cleanup_args_types_and_annotated;
    }
  }

  ast_typeexpr_init_copy(&args_types[args_count], partial_type);

  ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);

  struct ast_typeexpr funcexpr;
  funcexpr.tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                   make_ast_ident(func_ident),
                   args_types, args_types_count);

  struct ast_typeexpr resolved_funcexpr;
  int lvalue_discard;
  struct ast_expr annotated_funcexpr;
  if (!check_expr(es, x->func, &funcexpr,
                  &resolved_funcexpr, &lvalue_discard, &annotated_funcexpr)) {
    goto fail_cleanup_funcexpr;
  }

  ast_funcall_init(annotated_out, ast_meta_make_copy(&x->meta),
                   annotated_funcexpr, args_annotated, args_count);
  copy_func_return_type(es->cs->im, &resolved_funcexpr, args_types_count, out);

  ast_typeexpr_destroy(&funcexpr);
  return 1;
  /* Don't fall-through -- args_annotated was moved into annotated_out. */
 fail_cleanup_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  SLICE_FREE(args_annotated, args_count, ast_expr_destroy);
  return 0;
  /* Don't fall-through -- args_types was moved into funcexpr. */
 fail_cleanup_args_types_and_annotated:
  SLICE_FREE(args_types, i, ast_typeexpr_destroy);
  SLICE_FREE(args_annotated, i, ast_expr_destroy);
  return 0;
}

struct label_info {
  /* The name of the label in question. */
  ident_value label_name;
  /* True if the label has been observed. */
  int is_label_observed;
  /* True if a goto has been observed. */
  int is_goto_observed;
};

void label_info_wipe(struct label_info *a) {
  a->label_name = IDENT_VALUE_INVALID;
  a->is_label_observed = 0;
  a->is_goto_observed = 0;
}

struct bodystate {
  struct exprscope *es;
  struct ast_typeexpr *partial_type;
  int have_exact_return_type;
  struct ast_typeexpr exact_return_type;

  struct label_info *label_infos;
  size_t label_infos_count;
  size_t label_infos_limit;
};

void bodystate_init(struct bodystate *bs, struct exprscope *es,
                    struct ast_typeexpr *partial_type) {
  bs->es = es;
  bs->partial_type = partial_type;
  bs->have_exact_return_type = 0;
  bs->label_infos = NULL;
  bs->label_infos_count = 0;
  bs->label_infos_limit = 0;
}

void bodystate_destroy(struct bodystate *bs) {
  bs->es = NULL;
  if (bs->have_exact_return_type) {
    ast_typeexpr_destroy(&bs->exact_return_type);
    bs->have_exact_return_type = 0;
  }
  SLICE_FREE(bs->label_infos, bs->label_infos_count, label_info_wipe);
  bs->label_infos_limit = 0;
}

void bodystate_note_goto(struct bodystate *bs, ident_value target) {
  for (size_t i = 0, e = bs->label_infos_count; i < e; i++) {
    if (bs->label_infos[i].label_name == target) {
      bs->label_infos[i].is_goto_observed = 1;
      return;
    }
  }
  struct label_info info;
  info.label_name = target;
  info.is_label_observed = 0;
  info.is_goto_observed = 1;
  SLICE_PUSH(bs->label_infos, bs->label_infos_count, bs->label_infos_limit,
             info);
}

int bodystate_note_label(struct bodystate *bs, ident_value name) {
  for (size_t i = 0, e = bs->label_infos_count; i < e; i++) {
    if (bs->label_infos[i].label_name == name) {
      if (bs->label_infos[i].is_label_observed) {
        ERR_DBG("Duplicate label.\n");
        return 0;
      }
      bs->label_infos[i].is_label_observed = 1;
      return 1;
    }
  }
  struct label_info info;
  info.label_name = name;
  info.is_label_observed = 1;
  info.is_goto_observed = 0;
  SLICE_PUSH(bs->label_infos, bs->label_infos_count, bs->label_infos_limit,
             info);
  return 1;
}

void free_ast_vardecl(struct ast_vardecl **p) {
  ast_vardecl_destroy(*p);
  free(*p);
  *p = NULL;
}

int check_expr_bracebody(struct bodystate *bs,
                         struct ast_bracebody *x,
                         struct ast_bracebody *annotated_out) {
  struct ast_vardecl **vardecls_pushed = NULL;
  size_t vardecls_pushed_count = 0;
  size_t vardecls_pushed_limit = 0;
  int ret = 0;

  struct ast_statement *annotated_statements
    = malloc_mul(sizeof(*annotated_statements), x->statements_count);

  size_t i = 0;
  for (size_t e = x->statements_count; i < e; i++) {
    struct ast_statement *s = &x->statements[i];
    switch (s->tag) {
    case AST_STATEMENT_EXPR: {
      struct ast_typeexpr anything;
      anything.tag = AST_TYPEEXPR_UNKNOWN;
      struct ast_typeexpr discard;
      int lvalue_discard;
      struct ast_expr annotated_expr;
      if (!check_expr(bs->es, s->u.expr, &anything,
                      &discard, &lvalue_discard, &annotated_expr)) {
        goto fail;
      }
      annotated_statements[i].tag = AST_STATEMENT_EXPR;
      malloc_move_ast_expr(annotated_expr, &annotated_statements[i].u.expr);
      ast_typeexpr_destroy(&discard);
    } break;
    case AST_STATEMENT_RETURN_EXPR: {
      struct ast_typeexpr type;
      int lvalue_discard;
      struct ast_expr annotated_expr;
      if (!check_expr(bs->es, s->u.return_expr, bs->partial_type,
                      &type, &lvalue_discard, &annotated_expr)) {
        goto fail;
      }
      if (!bs->have_exact_return_type) {
        bs->have_exact_return_type = 1;
        bs->exact_return_type = type;
      } else {
        if (!exact_typeexprs_equal(&bs->exact_return_type, &type)) {
          ERR_DBG("Return statements with conflicting return types.\n");
          ast_expr_destroy(&annotated_expr);
          ast_typeexpr_destroy(&type);
          goto fail;
        } else {
          ast_typeexpr_destroy(&type);
        }
      }
      annotated_statements[i].tag = AST_STATEMENT_RETURN_EXPR;
      malloc_move_ast_expr(annotated_expr,
                           &annotated_statements[i].u.return_expr);
    } break;
    case AST_STATEMENT_VAR: {
      struct ast_typeexpr replaced_type;
      replace_generics(bs->es, &s->u.var_statement.decl.type, &replaced_type);

      struct ast_typeexpr rhs_type;
      int lvalue_discard;
      struct ast_expr annotated_rhs;
      if (!check_expr(bs->es, s->u.var_statement.rhs, &replaced_type,
                      &rhs_type, &lvalue_discard, &annotated_rhs)) {
        ast_typeexpr_destroy(&replaced_type);
        goto fail;
      }

      ast_typeexpr_destroy(&rhs_type);

      struct ast_ident name;
      ast_ident_init_copy(&name, &s->u.var_statement.decl.name);

      struct ast_typeexpr replaced_type_copy;
      ast_typeexpr_init_copy(&replaced_type_copy, &replaced_type);

      struct ast_vardecl *replaced_decl = malloc(sizeof(*replaced_decl));
      CHECK(replaced_decl);
      ast_vardecl_init(replaced_decl, ast_meta_make_garbage(), name,
                       replaced_type_copy);

      if (!exprscope_push_var(bs->es, replaced_decl)) {
        free_ast_vardecl(&replaced_decl);
        ast_typeexpr_destroy(&replaced_type);
        ast_expr_destroy(&annotated_rhs);
        goto fail;
      }

      SLICE_PUSH(vardecls_pushed, vardecls_pushed_count, vardecls_pushed_limit,
                 replaced_decl);

      struct ast_vardecl decl;
      ast_vardecl_init_copy(&decl, &s->u.var_statement.decl);
      annotated_statements[i].tag = AST_STATEMENT_VAR;
      ast_var_statement_init(&annotated_statements[i].u.var_statement,
                             ast_meta_make_copy(&s->u.var_statement.meta),
                             decl, annotated_rhs);
      ast_var_statement_info_note_type(
          &annotated_statements[i].u.var_statement.info,
          replaced_type);
    } break;
    case AST_STATEMENT_GOTO: {
      bodystate_note_goto(bs, s->u.goto_statement.target.value);
      ast_statement_init_copy(&annotated_statements[i], s);
    } break;
    case AST_STATEMENT_LABEL: {
      if (!bodystate_note_label(bs, s->u.label_statement.label.value)) {
        goto fail;
      }
      ast_statement_init_copy(&annotated_statements[i], s);
    } break;
    case AST_STATEMENT_IFTHEN: {
      struct ast_typeexpr boolean;
      init_boolean_typeexpr(bs->es->cs, &boolean);

      struct ast_typeexpr discard;
      int lvalue_discard;
      struct ast_expr annotated_condition;
      if (!check_expr(bs->es, s->u.ifthen_statement.condition, &boolean,
                      &discard, &lvalue_discard, &annotated_condition)) {
        ast_typeexpr_destroy(&boolean);
        goto fail;
      }
      ast_typeexpr_destroy(&boolean);
      ast_typeexpr_destroy(&discard);

      struct ast_bracebody annotated_thenbody;
      if (!check_expr_bracebody(bs, &s->u.ifthen_statement.thenbody,
                                &annotated_thenbody)) {
        ast_expr_destroy(&annotated_condition);
        goto fail;
      }

      annotated_statements[i].tag = AST_STATEMENT_IFTHEN;
      ast_ifthen_statement_init(
          &annotated_statements[i].u.ifthen_statement,
          ast_meta_make_copy(&s->u.ifthen_statement.meta),
          annotated_condition,
          annotated_thenbody);
    } break;
    case AST_STATEMENT_IFTHENELSE: {
      struct ast_typeexpr boolean;
      init_boolean_typeexpr(bs->es->cs, &boolean);

      struct ast_typeexpr discard;
      int lvalue_discard;
      struct ast_expr annotated_condition;
      if (!check_expr(bs->es, s->u.ifthenelse_statement.condition, &boolean,
                      &discard, &lvalue_discard, &annotated_condition)) {
        ast_typeexpr_destroy(&boolean);
        goto fail;
      }
      ast_typeexpr_destroy(&boolean);
      ast_typeexpr_destroy(&discard);

      struct ast_bracebody annotated_thenbody;
      if (!check_expr_bracebody(bs, &s->u.ifthenelse_statement.thenbody,
                                &annotated_thenbody)) {
        ast_expr_destroy(&annotated_condition);
        goto fail;
      }

      struct ast_bracebody annotated_elsebody;
      if (!check_expr_bracebody(bs, &s->u.ifthenelse_statement.elsebody,
                                &annotated_elsebody)) {
        ast_bracebody_destroy(&annotated_thenbody);
        ast_expr_destroy(&annotated_condition);
        goto fail;
      }

      annotated_statements[i].tag = AST_STATEMENT_IFTHENELSE;
      ast_ifthenelse_statement_init(
          &annotated_statements[i].u.ifthenelse_statement,
          ast_meta_make_copy(&s->u.ifthenelse_statement.meta),
          annotated_condition,
          annotated_thenbody,
          annotated_elsebody);
    } break;
    default:
      UNREACHABLE();
    }
  }

  ast_bracebody_init(annotated_out, ast_meta_make_copy(&x->meta),
                     annotated_statements, x->statements_count);
  ret = 1;
 fail:
  if (!ret) {
    SLICE_FREE(annotated_statements, i, ast_statement_destroy);
  }
  for (size_t j = 0; j < vardecls_pushed_count; j++) {
    exprscope_pop_var(bs->es);
  }
  SLICE_FREE(vardecls_pushed, vardecls_pushed_count, free_ast_vardecl);
  return ret;
}

int check_expr_funcbody(struct exprscope *es,
                        struct ast_bracebody *x,
                        struct ast_typeexpr *partial_type,
                        struct ast_typeexpr *out,
                        ident_value **label_names_out,
                        size_t *label_names_count_out,
                        struct ast_bracebody *annotated_out) {
  int ret = 0;

  struct bodystate bs;
  bodystate_init(&bs, es, partial_type);

  /* TODO: We need to analyze whether _all_ paths return.  Also, goto
     should be restricted from jumping to non-subset variable
     scopes. */
  struct ast_bracebody annotated_bracebody;
  if (!check_expr_bracebody(&bs, x, &annotated_bracebody)) {
    goto fail;
  }

  ident_value *label_names = malloc_mul(sizeof(*label_names),
                                        bs.label_infos_count);

  for (size_t i = 0; i < bs.label_infos_count; i++) {
    if (!bs.label_infos[i].is_label_observed) {
      ERR_DBG("goto without label.\n");
      goto fail_label_names;
    }
    if (!bs.label_infos[i].is_goto_observed) {
      ERR_DBG("label without goto.\n");
      goto fail_label_names;
    }
    label_names[i] = bs.label_infos[i].label_name;
  }

  if (!bs.have_exact_return_type) {
    ERR_DBG("Missing a return statement.\n");
    goto fail_label_names;
  }
  *out = bs.exact_return_type;
  bs.have_exact_return_type = 0;
  *annotated_out = annotated_bracebody;
  *label_names_out = label_names;
  *label_names_count_out = bs.label_infos_count;

  ret = 1;
 fail_label_names:
  if (!ret) {
    free(label_names);
    ast_bracebody_destroy(&annotated_bracebody);
  }
 fail:
  bodystate_destroy(&bs);
  return ret;
}

int check_expr_lambda(struct exprscope *es,
                      struct ast_lambda *x,
                      struct ast_typeexpr *partial_type,
                      struct ast_typeexpr *out,
                      struct ast_lambda *annotated_out) {
  CHECK_DBG("check_expr_lambda\n");
  ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);
  size_t func_params_count = x->params_count;
  size_t args_count = size_add(func_params_count, 1);

  struct ast_typeexpr funcexpr;
  {
    struct ast_typeexpr *args = malloc_mul(sizeof(*args), args_count);
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

    replace_generics(es, &x->return_type, &args[func_params_count]);

    funcexpr.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                     make_ast_ident(func_ident), args, args_count);
  }

  if (!unify_directionally(partial_type, &funcexpr)) {
    ERR_DBG("lambda type does not match expression type.\n");
    goto fail_funcexpr;
  }

  /* Because lambdas can't capture variables, we make a fresh exprscope. */

  struct ast_vardecl *replaced_vardecls
    = malloc_mul(sizeof(*replaced_vardecls), func_params_count);
  size_t replaced_vardecls_size = func_params_count;

  for (size_t i = 0; i < func_params_count; i++) {
    struct ast_typeexpr type;
    ast_typeexpr_init_copy(&type, &funcexpr.u.app.params[i]);
    ast_vardecl_init(&replaced_vardecls[i],
                     ast_meta_make_garbage(),
                     make_ast_ident(x->params[i].name.value),
                     type);
  }

  /* The funcbody does not need to be a statically computable
     expression, because the lambda is not evaluated. */
  struct exprscope bb_es;
  exprscope_init(&bb_es, es->cs, es->generics, es->generics_substitutions,
                 es->generics_substitutions_count,
                 STATIC_COMPUTATION_NO, NULL);

  for (size_t i = 0; i < func_params_count; i++) {
    int res = exprscope_push_var(&bb_es, &replaced_vardecls[i]);
    /* Pushing the var should succeed, because we already called
       check_var_shadowing above. */
    CHECK(res);
  }

  struct ast_typeexpr computed_return_type;
  ident_value *label_names;
  size_t label_names_count;
  struct ast_bracebody annotated_bracebody;
  if (!check_expr_funcbody(
          &bb_es,
          &x->bracebody,
          &funcexpr.u.app.params[size_sub(funcexpr.u.app.params_count, 1)],
          &computed_return_type,
          &label_names,
          &label_names_count,
          &annotated_bracebody)) {
    CHECK_DBG("check_expr_funcbody fails\n");
    goto fail_bb_es;
  }

  ast_typeexpr_destroy(&computed_return_type);
  exprscope_destroy(&bb_es);
  SLICE_FREE(replaced_vardecls, replaced_vardecls_size, ast_vardecl_destroy);
  *out = funcexpr;
  {
    struct ast_vardecl *params = malloc_mul(sizeof(*params), x->params_count);
    for (size_t i = 0, e = x->params_count; i < e; i++) {
      ast_vardecl_init_copy(&params[i], &x->params[i]);
    }
    struct ast_typeexpr return_type;
    ast_typeexpr_init_copy(&return_type, &x->return_type);

    ast_lambda_init(annotated_out, ast_meta_make_copy(&x->meta),
                    params, x->params_count,
                    return_type, annotated_bracebody);
    ast_lambda_info_set_labels(&annotated_out->info, label_names, label_names_count);
  }
  CHECK_DBG("check_expr_lambda succeeds\n");
  return 1;

 fail_bb_es:
  exprscope_destroy(&bb_es);
  SLICE_FREE(replaced_vardecls, replaced_vardecls_size, ast_vardecl_destroy);
 fail_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  return 0;
}

int check_expr_magic_binop(struct exprscope *es,
                           struct ast_binop_expr *x,
                           struct ast_typeexpr *partial_type,
                           struct ast_typeexpr *out,
                           int *is_lvalue_out,
                           struct ast_binop_expr *annotated_out) {
  int ret = 0;
  struct ast_typeexpr no_partial;
  no_partial.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_typeexpr lhs_type;
  int lhs_is_lvalue;
  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &no_partial, &lhs_type, &lhs_is_lvalue,
                  &annotated_lhs)) {
    goto cleanup;
  }

  struct ast_typeexpr rhs_type;
  int rhs_lvalue_discard;
  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &no_partial, &rhs_type, &rhs_lvalue_discard,
                  &annotated_rhs)) {
    goto cleanup_lhs_type;
  }

  switch (x->operator) {
  case AST_BINOP_ASSIGN: {
    if (es->computation == STATIC_COMPUTATION_YES) {
      ERR_DBG("Assignment within statically evaluated expression.\n");
      goto cleanup_both_types;
    }
    if (!lhs_is_lvalue) {
      ERR_DBG("Trying to assign to non-lvalue.\n");
      goto cleanup_both_types;
    }
    if (!exact_typeexprs_equal(&lhs_type, &rhs_type)) {
      ERR_DBG("Assignment with non-matching types.\n");
      goto cleanup_both_types;
    }

    if (!unify_directionally(partial_type, &lhs_type)) {
      ERR_DBG("LHS type of assignment does not match contextual type.\n");
      goto cleanup_both_types;
    }

    *out = lhs_type;
    *is_lvalue_out = 1;

    ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                        x->operator, annotated_lhs, annotated_rhs);

    ret = 1;
    goto cleanup_just_rhs_type;
  } break;
  case AST_BINOP_LOGICAL_OR:
  case AST_BINOP_LOGICAL_AND: {
    struct ast_typeexpr boolean;
    boolean.tag = AST_TYPEEXPR_NAME;
    boolean.u.name = make_ast_ident(
        identmap_intern_c_str(es->cs->im, BOOLEAN_STANDIN_TYPE_NAME));

    if (!unify_directionally(&boolean, &lhs_type)) {
      ERR_DBG("LHS of and/or is non-boolean.\n");
      ast_typeexpr_destroy(&boolean);
      goto cleanup_both_types;
    }

    if (!unify_directionally(&boolean, &rhs_type)) {
      ERR_DBG("RHS of and/or is non-boolean.\n");
      ast_typeexpr_destroy(&boolean);
      goto cleanup_both_types;
    }

    if (!unify_directionally(partial_type, &boolean)) {
      ERR_DBG("And/or expression in non-boolean context.\n");
      ast_typeexpr_destroy(&boolean);
      goto cleanup_both_types;
    }

    *out = boolean;
    *is_lvalue_out = 0;

    ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                        x->operator, annotated_lhs, annotated_rhs);

    ret = 1;
    goto cleanup_both_types;
  } break;
  default:
    UNREACHABLE();
  }

 cleanup_just_rhs_type:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
  ast_typeexpr_destroy(&rhs_type);
  goto cleanup;
 cleanup_both_types:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
  ast_typeexpr_destroy(&rhs_type);
 cleanup_lhs_type:
  if (!ret) {
    ast_expr_destroy(&annotated_lhs);
  }
  ast_typeexpr_destroy(&lhs_type);
 cleanup:
  return ret;
}

int is_statically_computable_non_magic_binop(enum ast_binop op) {
  (void)op;
  /* This will be incorrect when there's floats, because compile-time
     float computation is weird? */
  return 1;
}

int check_expr_binop(struct exprscope *es,
                     struct ast_binop_expr *x,
                     struct ast_typeexpr *partial_type,
                     struct ast_typeexpr *out,
                     int *is_lvalue_out,
                     struct ast_binop_expr *annotated_out) {
  if (is_magic_binop(x->operator)) {
    return check_expr_magic_binop(es, x, partial_type, out, is_lvalue_out,
                                  annotated_out);
  }

  if (es->computation == STATIC_COMPUTATION_YES
      && !is_statically_computable_non_magic_binop(x->operator)) {
    ERR_DBG("Non-statically computable binop.\n");
    return 0;
  }

  struct ast_typeexpr local_partial;
  local_partial.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_typeexpr lhs_type;
  int lvalue_discard;
  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &local_partial, &lhs_type, &lvalue_discard,
                  &annotated_lhs)) {
    goto fail;
  }

  struct ast_typeexpr rhs_type;
  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &local_partial, &rhs_type, &lvalue_discard,
                  &annotated_rhs)) {
    goto fail_cleanup_lhs_type;
  }

  struct ast_typeexpr *args_types = malloc_mul(sizeof(*args_types), 3);
  args_types[0] = lhs_type;
  args_types[1] = rhs_type;
  ast_typeexpr_init_copy(&args_types[2], partial_type);

  ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);

  struct ast_typeexpr funcexpr;
  funcexpr.tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                   make_ast_ident(func_ident), args_types, 3);

  int ret = 0;
  struct ast_typeexpr resolved_funcexpr;
  int funcexpr_lvalue_discard;
  struct def_instantiation *inst_discard;
  if (!lookup_global_maybe_typecheck(
          es,
          identmap_intern_c_str(es->cs->im,
                                binop_fakename(x->operator)),
          &funcexpr,
          &resolved_funcexpr,
          &funcexpr_lvalue_discard,
          &inst_discard)) {
    goto fail_cleanup_funcexpr;
  }

  copy_func_return_type(es->cs->im, &resolved_funcexpr, 3, out);
  *is_lvalue_out = 0;

  ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                      x->operator, annotated_lhs, annotated_rhs);

  ret = 1;
  ast_typeexpr_destroy(&resolved_funcexpr);
 fail_cleanup_funcexpr:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
    ast_expr_destroy(&annotated_lhs);
  }
  ast_typeexpr_destroy(&funcexpr);
  return ret;
  /* Don't fall-through -- lhs_type was moved into funcexpr. */
 fail_cleanup_lhs_type:
  ast_expr_destroy(&annotated_lhs);
  ast_typeexpr_destroy(&lhs_type);
 fail:
  return 0;
}

int view_ptr_target(struct identmap *im,
                    struct ast_typeexpr *ptr_type,
                    struct ast_typeexpr **target_out) {
  if (ptr_type->tag != AST_TYPEEXPR_APP) {
    return 0;
  }

  ident_value ptr_ident = identmap_intern_c_str(im, PTR_TYPE_NAME);
  if (ptr_type->u.app.name.value != ptr_ident) {
    return 0;
  }

  if (ptr_type->u.app.params_count != 1) {
    return 0;
  }

  *target_out = &ptr_type->u.app.params[0];
  return 1;
}

void wrap_in_ptr(struct identmap *im,
                 struct ast_typeexpr *target,
                 struct ast_typeexpr *ptr_out) {
  ptr_out->tag = AST_TYPEEXPR_APP;
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), 1);
  ast_typeexpr_init_copy(&params[0], target);
  ast_typeapp_init(&ptr_out->u.app, ast_meta_make_garbage(),
                   make_ast_ident(identmap_intern_c_str(im, PTR_TYPE_NAME)),
                   params, 1);
}

int check_expr_magic_unop(struct exprscope *es,
                          struct ast_unop_expr *x,
                          struct ast_typeexpr *partial_type,
                          struct ast_typeexpr *out,
                          int *is_lvalue_out,
                          struct ast_unop_expr *annotated_out) {
  if (es->computation == STATIC_COMPUTATION_YES) {
    ERR_DBG("Magic unops not allowed in static expressions.\n");
    return 0;
  }

  int ret = 0;
  struct ast_typeexpr no_partial;
  no_partial.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_typeexpr rhs_type;
  int rhs_is_lvalue;
  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &no_partial, &rhs_type, &rhs_is_lvalue,
                  &annotated_rhs)) {
    return 0;
  }

  switch (x->operator) {
  case AST_UNOP_DEREFERENCE: {
    struct ast_typeexpr *rhs_target;
    if (!view_ptr_target(es->cs->im, &rhs_type, &rhs_target)) {
      ERR_DBG("Trying to dereference a non-pointer.\n");
      goto cleanup_rhs_type;
    }

    if (!unify_directionally(partial_type, rhs_target)) {
      ERR_DBG("Pointer dereference results in wrong type.\n");
      goto cleanup_rhs_type;
    }

    ast_typeexpr_init_copy(out, rhs_target);
    *is_lvalue_out = 1;
    ast_unop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                       x->operator, annotated_rhs);
    ret = 1;
  } break;
  case AST_UNOP_ADDRESSOF: {
    if (!rhs_is_lvalue) {
      ERR_DBG("Trying to take the address of a non-lvalue.\n");
      goto cleanup_rhs_type;
    }

    struct ast_typeexpr pointer_type;
    wrap_in_ptr(es->cs->im, &rhs_type, &pointer_type);

    if (!unify_directionally(partial_type, &pointer_type)) {
      ERR_DBG("Addressof results in wrong type.\n");
      ast_typeexpr_destroy(&pointer_type);
      goto cleanup_rhs_type;
    }

    *out = pointer_type;
    *is_lvalue_out = 0;
    ast_unop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                       x->operator, annotated_rhs);
    ret = 1;
  } break;
  default:
    UNREACHABLE();
  }

 cleanup_rhs_type:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
  ast_typeexpr_destroy(&rhs_type);
  return ret;
}

int non_magic_unop_statically_computable(enum ast_unop operator) {
  return operator == AST_UNOP_NEGATE;
}

int check_expr_unop(struct exprscope *es,
                    struct ast_unop_expr *x,
                    struct ast_typeexpr *partial_type,
                    struct ast_typeexpr *out,
                    int *is_lvalue_out,
                    struct ast_unop_expr *annotated_out) {
  int ret = 0;
  if (is_magic_unop(x->operator)) {
    return check_expr_magic_unop(es, x, partial_type, out, is_lvalue_out,
                                 annotated_out);
  }

  if (es->computation == STATIC_COMPUTATION_YES
      && !non_magic_unop_statically_computable(x->operator)) {
    ERR_DBG("Non-statically-computable unop function.\n");
    return 0;
  }

  struct ast_typeexpr local_partial;
  local_partial.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_typeexpr rhs_type;
  int lvalue_discard;
  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &local_partial, &rhs_type, &lvalue_discard,
                  &annotated_rhs)) {
    goto fail;
  }

  struct ast_typeexpr *args_types = malloc_mul(sizeof(*args_types), 3);
  args_types[0] = rhs_type;
  ast_typeexpr_init_copy(&args_types[1], partial_type);

  ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);

  struct ast_typeexpr funcexpr;
  funcexpr.tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                   make_ast_ident(func_ident), args_types, 2);

  struct ast_typeexpr resolved_funcexpr;
  int funcexpr_lvalue_discard;
  struct def_instantiation *inst_discard;
  if (!lookup_global_maybe_typecheck(
          es,
          identmap_intern_c_str(es->cs->im, unop_fakename(x->operator)),
          &funcexpr,
          &resolved_funcexpr,
          &funcexpr_lvalue_discard,
          &inst_discard)) {
    goto cleanup_funcexpr;
  }

  copy_func_return_type(es->cs->im, &resolved_funcexpr, 2, out);
  *is_lvalue_out = 0;
  ast_unop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                     x->operator, annotated_rhs);

  ret = 1;
  ast_typeexpr_destroy(&resolved_funcexpr);
 cleanup_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
 fail:
  return ret;
}

int lookup_fields_field_type(struct ast_vardecl *fields,
                             size_t fields_count,
                             ident_value field_name,
                             struct ast_typeexpr *field_type_out) {
  for (size_t i = 0; i < fields_count; i++) {
    if (fields[i].name.value == field_name) {
      ast_typeexpr_init_copy(field_type_out, &fields[i].type);
      return 1;
    }
  }

  ERR_DBG("Field name not found.\n");
  return 0;
}

int lookup_field_type(struct exprscope *es,
                      struct ast_typeexpr *type,
                      ident_value field_name,
                      struct ast_typeexpr *field_type_out) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&es->cs->nt, type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      CRASH("lookup_field_type sees an invalid type.\n");
    }
    if (ent->is_primitive) {
      ERR_DBG("Looking up field on primitive type.\n");
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(!deftype->generics.has_type_params);
    return lookup_field_type(es, &deftype->type, field_name, field_type_out);
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&es->cs->nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params_count),
                                   &ent)) {
      CRASH("lookup_field_type sees an invalid generic type.\n");
    }
    if (ent->is_primitive) {
      ERR_DBG("Lookup up field on primitive type.\n");
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype->generics.has_type_params
          && deftype->generics.params_count == type->u.app.params_count);

    struct ast_typeexpr concrete_deftype_type;
    do_replace_generics(&deftype->generics,
                        type->u.app.params,
                        &deftype->type,
                        &concrete_deftype_type);

    int ret = lookup_field_type(es, &concrete_deftype_type, field_name,
                                field_type_out);
    ast_typeexpr_destroy(&concrete_deftype_type);
    return ret;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return lookup_fields_field_type(type->u.structe.fields,
                                    type->u.structe.fields_count, field_name,
                                    field_type_out);
  case AST_TYPEEXPR_UNIONE:
    return lookup_fields_field_type(type->u.unione.fields,
                                    type->u.unione.fields_count, field_name,
                                    field_type_out);
  default:
    UNREACHABLE();
  }
}

int check_expr_local_field_access(
    struct exprscope *es,
    struct ast_local_field_access *x,
    struct ast_typeexpr *partial_type,
    struct ast_typeexpr *out,
    int *is_lvalue_out,
    struct ast_local_field_access *annotated_out) {
  int ret = 0;
  struct ast_typeexpr lhs_partial_type;
  lhs_partial_type.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_typeexpr lhs_type;
  int lhs_lvalue;
  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &lhs_partial_type, &lhs_type, &lhs_lvalue,
                  &annotated_lhs)) {
    goto cleanup;
  }

  struct ast_typeexpr field_type;
  if (!lookup_field_type(es, &lhs_type, x->fieldname.value, &field_type)) {
    goto cleanup_lhs_type;
  }

  if (!unify_directionally(partial_type, &field_type)) {
    goto cleanup_field_type;
  }

  *out = field_type;
  *is_lvalue_out = lhs_lvalue;
  struct ast_ident fieldname;
  ast_ident_init_copy(&fieldname, &x->fieldname);
  ast_local_field_access_init(annotated_out, ast_meta_make_copy(&x->meta),
                              annotated_lhs, fieldname);
  ret = 1;
  goto cleanup_lhs_type;

 cleanup_field_type:
  ast_typeexpr_destroy(&field_type);
 cleanup_lhs_type:
  if (!ret) {
    ast_expr_destroy(&annotated_lhs);
  }
  ast_typeexpr_destroy(&lhs_type);
 cleanup:
  return ret;
}

int check_expr_deref_field_access(
    struct exprscope *es,
    struct ast_deref_field_access *x,
    struct ast_typeexpr *partial_type,
    struct ast_typeexpr *out,
    int *is_lvalue_out,
    struct ast_deref_field_access *annotated_out) {
  if (es->computation == STATIC_COMPUTATION_YES) {
    ERR_DBG("Dereferencing field access disallowed in static computation.\n");
    return 0;
  }
  int ret = 0;
  /* Even though we know the lhs is supposed to be a ptr, we shouldn't
     put that info into the context when type checking it. */
  struct ast_typeexpr lhs_partial_type;
  lhs_partial_type.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_typeexpr lhs_type;
  int lvalue_discard;
  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &lhs_partial_type,
                  &lhs_type, &lvalue_discard, &annotated_lhs)) {
    goto cleanup;
  }

  struct ast_typeexpr *ptr_target;
  if (!view_ptr_target(es->cs->im, &lhs_type, &ptr_target)) {
    ERR_DBG("Dereferencing field access expects ptr type.\n");
    goto cleanup_lhs_type;
  }

  struct ast_typeexpr field_type;
  if (!lookup_field_type(es, ptr_target, x->fieldname.value, &field_type)) {
    goto cleanup_lhs_type;
  }

  if (!unify_directionally(partial_type, &field_type)) {
    ERR_DBG("Dereferencing field access results in wrong type.\n");
    goto cleanup_field_type;
  }

  *out = field_type;
  *is_lvalue_out = 1;
  struct ast_ident fieldname;
  ast_ident_init_copy(&fieldname, &x->fieldname);
  ast_deref_field_access_init(annotated_out, ast_meta_make_copy(&x->meta),
                              annotated_lhs, fieldname);
  ret = 1;
  goto cleanup_lhs_type;

 cleanup_field_type:
  ast_typeexpr_destroy(&field_type);
 cleanup_lhs_type:
  if (!ret) {
    ast_expr_destroy(&annotated_lhs);
  }
  ast_typeexpr_destroy(&lhs_type);
 cleanup:
  return ret;
}

int check_expr(struct exprscope *es,
               struct ast_expr *x,
               struct ast_typeexpr *partial_type,
               struct ast_typeexpr *out,
               int *is_lvalue_out,
               struct ast_expr *annotated_out) {
  switch (x->tag) {
  case AST_EXPR_NAME: {
    struct ast_typeexpr name_type;
    int is_lvalue;
    struct def_instantiation *inst_or_null;
    if (!exprscope_lookup_name(es, x->u.name.ident.value, partial_type,
                               &name_type, &is_lvalue, &inst_or_null)) {
      return 0;
    }
    *is_lvalue_out = is_lvalue;
    ast_expr_partial_init(annotated_out, AST_EXPR_NAME, ast_expr_info_typechecked(name_type));
    ast_name_expr_init_copy(&annotated_out->u.name, &x->u.name);
    ast_name_expr_info_mark_inst(&annotated_out->u.name.info, inst_or_null);
    goto success;
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    struct ast_typeexpr num_type;
    numeric_literal_type(es->cs->im, &x->u.numeric_literal, &num_type);
    if (!unify_directionally(partial_type, &num_type)) {
      ERR_DBG("Numeric literal in bad place.\n");
      ast_typeexpr_destroy(&num_type);
      return 0;
    }
    *is_lvalue_out = 0;
    ast_expr_partial_init(annotated_out, AST_EXPR_NUMERIC_LITERAL,
                          ast_expr_info_typechecked(num_type));
    ast_numeric_literal_init_copy(&annotated_out->u.numeric_literal,
                                  &x->u.numeric_literal);
    goto success;
  } break;
  case AST_EXPR_FUNCALL: {
    struct ast_typeexpr return_type;
    if (!check_expr_funcall(es, &x->u.funcall, partial_type,
                            &return_type, &annotated_out->u.funcall)) {
      return 0;
    }
    *is_lvalue_out = 0;
    ast_expr_partial_init(annotated_out, AST_EXPR_FUNCALL,
                          ast_expr_info_typechecked(return_type));
    goto success;
  } break;
  case AST_EXPR_UNOP: {
    struct ast_typeexpr return_type;
    if (!check_expr_unop(es, &x->u.unop_expr, partial_type,
                         &return_type, is_lvalue_out, &annotated_out->u.unop_expr)) {
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_UNOP,
                          ast_expr_info_typechecked(return_type));
    goto success;
  } break;
  case AST_EXPR_BINOP: {
    struct ast_typeexpr return_type;
    if (!check_expr_binop(es, &x->u.binop_expr, partial_type,
                          &return_type, is_lvalue_out, &annotated_out->u.binop_expr)) {
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_BINOP,
                          ast_expr_info_typechecked(return_type));
    goto success;
  } break;
  case AST_EXPR_LAMBDA: {
    struct ast_typeexpr type;
    if (!check_expr_lambda(es, &x->u.lambda, partial_type,
                           &type, &annotated_out->u.lambda)) {
      return 0;
    }
    *is_lvalue_out = 0;
    ast_expr_partial_init(annotated_out, AST_EXPR_LAMBDA,
                          ast_expr_info_typechecked(type));
    goto success;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    struct ast_typeexpr type;
    if (!check_expr_local_field_access(es, &x->u.local_field_access,
                                       partial_type, &type, is_lvalue_out,
                                       &annotated_out->u.local_field_access)) {
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_LOCAL_FIELD_ACCESS,
                          ast_expr_info_typechecked(type));
    goto success;
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    struct ast_typeexpr type;
    if (!check_expr_deref_field_access(es, &x->u.deref_field_access,
                                       partial_type, &type, is_lvalue_out,
                                       &annotated_out->u.deref_field_access)) {
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_DEREF_FIELD_ACCESS,
                          ast_expr_info_typechecked(type));
    goto success;
  } break;
  default:
    UNREACHABLE();
  }

 success:
  ast_typeexpr_init_copy(out, &annotated_out->expr_info.concrete_type);
  return 1;
}

/* Checks an expr, given that we know the type of expr. */
int check_expr_with_type(struct exprscope *es,
                         struct ast_expr *x,
                         struct ast_typeexpr *type,
                         struct ast_expr *annotated_out) {
  CHECK_DBG("check_expr_with_type\n");
  struct ast_typeexpr out;
  int lvalue_discard;
  int ret = check_expr(es, x, type, &out, &lvalue_discard, annotated_out);
  if (ret) {
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
    struct ast_typeexpr unified;
    struct def_entry *ent;
    struct def_instantiation *inst;
    int success = name_table_match_def(&cs->nt,
                                       a->name.value,
                                       NULL, 0, /* (no generics) */
                                       &a->type,
                                       &unified,
                                       &ent,
                                       &inst);
    CHECK(success);
    CHECK(exact_typeexprs_equal(&unified, &a->type));
    CHECK(!ent->is_primitive);

    int ret;
    if (!inst->typecheck_started) {
      CHECK(!inst->annotated_rhs_computed);
      inst->typecheck_started = 1;
      struct exprscope es;
      exprscope_init(&es, cs, &a->generics, NULL, 0,
                     STATIC_COMPUTATION_YES, ent);
      struct ast_expr annotated_rhs;
      ret = check_expr_with_type(&es, &a->rhs, &a->type, &annotated_rhs);
      if (ret) {
        CHECK(!inst->annotated_rhs_computed);
        inst->annotated_rhs_computed = 1;
        inst->annotated_rhs = annotated_rhs;
      }
      exprscope_destroy(&es);
    } else {
      ret = 1;
    }

    return ret;
  } else {
    return 1;
  }
}

int check_extern_def(struct checkstate *cs, struct ast_extern_def *a) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  int ret = check_typeexpr(cs, &generics, &a->type, NULL);
  ast_generics_destroy(&generics);
  return ret;
}

int check_toplevel(struct checkstate *cs, struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    /* We already parsed and loaded the import. */
    return 1;
  case AST_TOPLEVEL_DEF:
    return check_def(cs, &a->u.def);
  case AST_TOPLEVEL_EXTERN_DEF:
    return check_extern_def(cs, &a->u.extern_def);
  case AST_TOPLEVEL_DEFTYPE:
    return check_deftype(cs, lookup_deftype(&cs->nt, &a->u.deftype));
  default:
    UNREACHABLE();
  }
}

#define NUMERIC_LITERAL_OOR "Numeric literal out of range.\n"

int numeric_literal_to_u32(int8_t *digits, size_t digits_count,
                           uint32_t *out) {
  CHECK(digits_count > 0);
  uint32_t built_value = 0;
  for (size_t i = 0; i < digits_count; i++) {
    if (!try_uint32_mul(built_value, 10, &built_value)) {
      ERR_DBG(NUMERIC_LITERAL_OOR);
      return 0;
    }
    if (!try_uint32_add(built_value, digits[i], &built_value)) {
      ERR_DBG(NUMERIC_LITERAL_OOR);
      return 0;
    }
  }

  *out = built_value;
  return 1;
}

int numeric_literal_to_i32(int8_t *digits, size_t digits_count,
                           int32_t *out) {
  /* TODO: There's no way to plainly represent INT32_MIN.  We should
     get static evaluation of "arbitrary numeric constants"
     implemented. */
  uint32_t value;
  if (!numeric_literal_to_u32(digits, digits_count, &value)) {
    return 0;
  }
  if (value > 0x7FFFFFFFul) {
    ERR_DBG(NUMERIC_LITERAL_OOR);
    return 0;
  }
  CHECK(value <= INT32_MAX);
  *out = (int32_t)value;
  return 1;
}

int eval_static_numeric_literal(struct ast_numeric_literal *a,
                                struct static_value *out) {
  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED: {
    int32_t value;
    if (!numeric_literal_to_i32(a->digits, a->digits_count, &value)) {
      return 0;
    }
    static_value_init_i32(out, value);
    return 1;
  } break;
  case AST_NUMERIC_TYPE_UNSIGNED: {
    uint32_t value;
    if (!numeric_literal_to_u32(a->digits, a->digits_count, &value)) {
      return 0;
    }
    static_value_init_u32(out, value);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int eval_static_value(struct ast_expr *expr, struct static_value *out);

int eval_static_unop_expr(struct ast_unop_expr *expr,
                          struct static_value *out) {
  CHECK(!is_magic_unop(expr->operator));
  /* Negation is the only non-magic unop right now. */
  CHECK(expr->operator == AST_UNOP_NEGATE);
  struct static_value rhs_value;
  if (!eval_static_value(expr->rhs, &rhs_value)) {
    return 0;
  }

  /* You can't negate unsigned ints -- it won't typecheck. */
  CHECK(rhs_value.tag == STATIC_VALUE_I32);
  if (rhs_value.u.i32_value == INT32_MIN) {
    ERR_DBG("Attempted negation of i32 min value (in static computation)\n");
    static_value_destroy(&rhs_value);
    return 0;
  }
  static_value_init_i32(out, -rhs_value.u.i32_value);
  static_value_destroy(&rhs_value);
  return 1;
}

int apply_operator(enum ast_binop op,
                   struct static_value *lhs,
                   struct static_value *rhs,
                   struct static_value *out) {
  CHECK(lhs->tag == rhs->tag);
  CHECK(lhs->tag == STATIC_VALUE_I32 || lhs->tag == STATIC_VALUE_U32);
  switch (lhs->tag) {
  case STATIC_VALUE_I32: {
    int32_t left = lhs->u.i32_value;
    int32_t right = rhs->u.i32_value;
    int32_t value = -12345;
    int success = 1;
    switch (op) {
    case AST_BINOP_ASSIGN: UNREACHABLE();
    case AST_BINOP_ADD:
      success = try_int32_add(left, right, &value);
      break;
    case AST_BINOP_SUB:
      success = try_int32_sub(left, right, &value);
      break;
    case AST_BINOP_MUL:
      success = try_int32_mul(left, right, &value);
      break;
    case AST_BINOP_DIV: {
      if (left < 0 || right < 0) {
        ERR_DBG("Negative static value division.\n");
        return 0;
      }
      if (right == 0) {
        ERR_DBG("Static value division by zero.\n");
      }
      value = int32_div(left, right);
      success = 1;
    } break;
    case AST_BINOP_MOD: {
      if (left < 0 || right < 0) {
        ERR_DBG("Negative static value modulo.\n");
        return 0;
      }
      if (right == 0) {
        ERR_DBG("Static value modulo by zero.\n");
      }
      value = int32_positive_mod(left, right);
      success = 1;
    } break;
    case AST_BINOP_LT:
      value = (left < right);
      break;
    case AST_BINOP_LE:
      value = (left <= right);
      break;
    case AST_BINOP_GT:
      value = (left > right);
      break;
    case AST_BINOP_GE:
      value = (left >= right);
      break;
    case AST_BINOP_EQ:
      value = (left == right);
      break;
    case AST_BINOP_NE:
      value = (left != right);
      break;
    case AST_BINOP_BIT_XOR:
      value = (left ^ right);
      break;
    case AST_BINOP_BIT_OR:
      value = (left | right);
      break;
    case AST_BINOP_BIT_AND:
      value = (left & right);
      break;
    case AST_BINOP_BIT_LEFTSHIFT: {
      if (left < 0 || right < 0 || right >= 32) {
        success = 0;
      } else {
        int64_t left64 = left;
        left64 <<= right;
        if (left64 > INT32_MAX) {
          success = 0;
        } else {
          value = (int32_t)left64;
        }
      }
    } break;
    case AST_BINOP_BIT_RIGHTSHIFT: {
      if (left < 0 || right < 0 || right >= 32) {
        ERR_DBG("Invalid i32 right-shift.\n");
      } else {
        value = (left >> right);
      }
    } break;
    default:
      success = 0;
    }

    if (!success) {
      ERR_DBG("Binary operation cannot be statically evaluated.\n");
      return 0;
    }

    static_value_init_i32(out, value);
    return 1;
  } break;

  case STATIC_VALUE_U32: {
    uint32_t left = lhs->u.u32_value;
    uint32_t right = rhs->u.u32_value;
    uint32_t value = 12345;
    int success = 1;
    switch (op) {
    case AST_BINOP_ASSIGN: UNREACHABLE();
    case AST_BINOP_ADD:
      success = try_uint32_add(left, right, &value);
      break;
    case AST_BINOP_SUB:
      success = try_uint32_sub(left, right, &value);
      break;
    case AST_BINOP_MUL:
      success = try_uint32_mul(left, right, &value);
      break;
    case AST_BINOP_DIV:
      success = try_uint32_div(left, right, &value);
      break;
    case AST_BINOP_MOD:
      success = try_uint32_mod(left, right, &value);
      break;
    case AST_BINOP_LT:
      value = (left < right);
      break;
    case AST_BINOP_LE:
      value = (left <= right);
      break;
    case AST_BINOP_GT:
      value = (left > right);
      break;
    case AST_BINOP_GE:
      value = (left >= right);
      break;
    case AST_BINOP_EQ:
      value = (left == right);
      break;
    case AST_BINOP_NE:
      value = (left != right);
      break;
    case AST_BINOP_BIT_XOR:
      value = (left ^ right);
      break;
    case AST_BINOP_BIT_OR:
      value = (left | right);
      break;
    case AST_BINOP_BIT_AND:
      value = (left & right);
      break;
    case AST_BINOP_BIT_LEFTSHIFT: {
      if (right >= 32) {
        success = 0;
      } else {
        uint64_t left64 = left;
        left64 <<= right;
        if (left64 > UINT32_MAX) {
          success = 0;
        } else {
          value = (uint32_t)left64;
        }
      }
    } break;
    case AST_BINOP_BIT_RIGHTSHIFT: {
      if (right >= 32) {
        success = 0;
      } else {
        value = (left >> right);
      }
    } break;
    default:
      success = 0;
    }

    if (!success) {
      ERR_DBG("Binary operation cannot be statically evaluated.\n");
      return 0;
    }

    static_value_init_u32(out, value);
    return 1;
  } break;
  case STATIC_VALUE_LAMBDA:
  default:
    UNREACHABLE();
  }
}

int eval_static_binop_expr(struct ast_binop_expr *expr,
                           struct static_value *out) {
  CHECK(!is_magic_binop(expr->operator));

  struct static_value lhs_value;
  if (!eval_static_value(expr->lhs, &lhs_value)) {
    goto fail;
  }

  struct static_value rhs_value;
  if (!eval_static_value(expr->rhs, &rhs_value)) {
    goto fail_lhs;
  }

  if (!apply_operator(expr->operator, &lhs_value, &rhs_value, out)) {
    goto fail_both;
  }

  return 1;
 fail_both:
  static_value_destroy(&rhs_value);
 fail_lhs:
  static_value_destroy(&lhs_value);
 fail:
  return 0;
}

/* expr must have been annotated by typechecking. */
int eval_static_value(struct ast_expr *expr,
                      struct static_value *out) {
  switch (expr->tag) {
  case AST_EXPR_NAME: {
    struct def_instantiation *inst_or_null;
    if (!ast_name_expr_info_get_inst(&expr->u.name.info,
                                     &inst_or_null)) {
      CRASH("Could not lookup instantation.");
    }
    CHECK(inst_or_null);
    CHECK(inst_or_null->value_computed);
    static_value_init_copy(out, &inst_or_null->value);
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL:
    return eval_static_numeric_literal(&expr->u.numeric_literal, out);
  case AST_EXPR_FUNCALL: {
    CRASH("No funcalls should have been deemed statically evaluable.\n");
  } break;
  case AST_EXPR_UNOP: {
    return eval_static_unop_expr(&expr->u.unop_expr, out);
  } break;
  case AST_EXPR_BINOP: {
    return eval_static_binop_expr(&expr->u.binop_expr, out);
  } break;
  case AST_EXPR_LAMBDA: {
    struct ast_expr copy;
    ast_expr_init_copy(&copy, expr);
    static_value_init_typechecked_lambda(out, copy);
    return 1;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    CRASH("No local field access expression should have been deemed "
          "statically evaluable.\n");
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    CRASH("No deref field access expression should have been deemed"
          "statically evaluable.\n");
  } break;
  default:
    UNREACHABLE();
  }
}

int compute_static_values(struct def_entry *ent) {
  CHECK(ent->def);
  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations[i];
    CHECK(!inst->value_computed);

    CHECK(inst->annotated_rhs_computed);
    if (!eval_static_value(&inst->annotated_rhs, &inst->value)) {
      return 0;
    }
    inst->value_computed = 1;
  }

  return 1;
}

int chase_def_entry_acyclicity(struct def_entry *ent) {
  if (ent->known_acyclic) {
    return 1;
  }
  if (ent->acyclicity_being_chased) {
    ERR_DBG("Cyclic reference in static expressions.\n");
    return 0;
  }
  ent->acyclicity_being_chased = 1;
  for (size_t i = 0, e = ent->static_references_count; i < e; i++) {
    if (!chase_def_entry_acyclicity(ent->static_references[i])) {
      return 0;
    }
  }
  CHECK(ent->acyclicity_being_chased == 1);
  ent->acyclicity_being_chased = 0;
  CHECK(ent->known_acyclic == 0);
  ent->known_acyclic = 1;

  if (!ent->is_primitive && !ent->is_extern) {
    if (!compute_static_values(ent)) {
      return 0;
    }
  }

  return 1;
}

int check_def_acyclicity(struct checkstate *cs) {
  for (size_t i = 0, e = cs->nt.defs_count; i < e; i++) {
    struct def_entry *ent = cs->nt.defs[i];
    if (!chase_def_entry_acyclicity(ent)) {
      return 0;
    }
  }
  return 1;
}

int chase_modules_and_typecheck(struct checkstate *cs,
                                module_loader *loader,
                                ident_value first_module) {
  checkstate_import_primitives(cs);

  int ret = 0;
  if (!chase_imports(cs, loader, first_module)) {
    goto fail;
  }

  for (size_t i = 0, e = cs->imports_count; i < e; i++) {
    struct ast_file *file = cs->imports[i].file;
    for (size_t j = 0, f = file->toplevels_count; j < f; j++) {
      if (!check_toplevel(cs, &file->toplevels[j])) {
        goto fail;
      }
    }
  }

  if (!check_def_acyclicity(cs)) {
    goto fail;
  }

  ret = 1;
 fail:
  return ret;
}

int test_check_module(struct identmap *im, module_loader *loader,
                      ident_value name) {
  struct checkstate cs;
  checkstate_init(&cs, im);

  int ret = chase_modules_and_typecheck(&cs, loader, name);

  checkstate_destroy(&cs);
  return ret;
}

int read_module_file(const uint8_t *module_name,
                     size_t module_name_count,
                     uint8_t **data_out,
                     size_t *data_size_out) {
  int ret = 0;
  char *filename;
  size_t filename_count;
  alloc_half_strcat(module_name, module_name_count,
                    ".ki",
                    &filename, &filename_count);

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
      uint8_t *data = malloc_mul(data_count, sizeof(uint8_t));
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
                               "def y u32 = 5u;\n" } };

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
                               "deftype foo struct { "
                               "x u32; y i32; z ptr[foo]; };\n" } };

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
                               "deftype[T] foo struct { "
                               "count u32; p ptr[T]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_7(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  /* This fails because bar recursively holds itself through a
     template parameter. */
  struct test_module a[] = { { "foo",
                               "deftype[T, U] foo struct { x ptr[T]; y U; };\n"
                               "deftype bar struct { z foo[u32, bar]; };\n" }
  };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_8(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  /* But here bar holds itself indirectly. */
  struct test_module a[] = { { "foo",
                               "deftype[T, U] foo struct { x ptr[T]; y U; };\n"
                               "deftype bar struct { z foo[bar, u32]; };\n" }
  };

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
  struct test_module a[] = { { "foo",
                               "def[] x i32 = 3;\n"
                               "def y i32 = x;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_1(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_2(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  if (z) {\n"
                               "    goto foo;\n"
                               "    var k i32 = y(x);\n"
                               "  } else {\n"
                               "    return x;\n"
                               "  }\n"
                               "  label foo;\n"
                               "  return z;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_3(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because there's no label named foo. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  if (z) {\n"
                               "    y;\n"
                               "    goto foo;\n"
                               "  } else {\n"
                               "    return x;\n"
                               "  }\n"
                               "  return z;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}


int check_file_test_lambda_4(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because k is a u32. */
  struct test_module a[] = { { "foo",
                               "def k u32 = k;\n"
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return k;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_5(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because x shadows a global. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  var x i32 = 4;\n"
                               "  return z;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_6(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because z shadows a local. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  var z i32 = 4;\n"
                               "  return x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_7(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return x + z + 5;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_8(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because x is a u32. */
  struct test_module a[] = { { "foo",
                               "def x u32 = x;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return x + z + 5;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_9(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return -x + z + -5;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_10(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because you can't negate a u32. */
  struct test_module a[] = { { "foo",
                               "def x u32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return -x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_11(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "deftype foo struct { x i32; y i32; };\n"
                               "def y func[foo, i32] = fn(z foo) i32 {\n"
                               "  return z.x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_12(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the field x has type u32. */
  struct test_module a[] = { { "foo",
                               "deftype foo struct { x u32; y i32; };\n"
                               "def y func[foo, i32] = fn(z foo) i32 {\n"
                               "  return z.x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_13(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y func[foo[i32], i32] = fn(z foo[i32]) i32 {\n"
      "  return z.x + z.y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_14(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because z.x is a u32. */
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y func[foo[u32], i32] = fn(z foo[u32]) i32 {\n"
      "  return z.x + z.y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_15(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def y func[i32, i32] = fn(z i32) i32 {\n"
                               "  var k func[i32, i32] = fn(m i32) i32 {\n"
                               "    return m + m;\n"
                               "  };\n"
                               "  return k(z) + k(z);\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_16(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the inner lambda tries to capture "z". */
  struct test_module a[] = { { "foo",
                               "def y func[i32, i32] = fn(z i32) i32 {\n"
                               "  var k func[i32, i32] = fn(m i32) i32 {\n"
                               "    return m + z;\n"
                               "  };\n"
                               "  return k(z) + k(z);\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_17(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype foo struct { x i32; y i32; };\n"
      "def y func[ptr[foo], i32] = fn(z ptr[foo]) i32 {\n"
      "  return z->x;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_18(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
      "  return z->x + z->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_19(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_20(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
      "  if (z->x < 3 && z->y > 19) {\n"
      "    z->x = (*z).y + 5;\n"
      "  }\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_21(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because assignment mismatches types. */
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
      "  if (z->x < 3 && z->y > 19) {\n"
      "    z->x = z;\n"
      "  }\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_22(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] foo func[ptr[T], T] = fn(x ptr[T]) T { return *x; };\n"
      "def bar func[i32] = fn() i32 {\n"
      "  var x i32 = 3;\n"
      "  return foo(&x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_23(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the def does not match. */
  struct test_module a[] = { {
      "foo",
      "def[T] foo func[ptr[T], T] = fn(x ptr[T]) T { return *x; };\n"
      "def bar func[i32] = fn() i32 {\n"
      "  var x i32 = 3;\n"
      "  return foo(x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_24(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] fac func[T, T] = fn(x T) T {\n"
      "  if (x == 0) {\n"
      "    return 1;\n"
      "  } else {\n"
      "    return x * fac(x - 1);\n"
      "  }\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
      "  var x i32 = 5;\n"
      "  return fac(x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_25(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because of recursive template instantiation. */
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x i32; };\n"
      "def[T] biggefy func[T, foo[T]] = fn(x T) foo[T] {\n"
      "  return biggefy(x);\n"
      "};\n"
      "def[T] rec func[T, i32] = fn(x T) i32 {\n"
      "  return rec(biggefy(x));\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
      "  var x u32 = 5u;\n"
      "  return rec(x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_26(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def x i32 = 3;\n"
      "def y u32 = 3u + 4u;\n"
      "def z func[i32, i32] = fn(k i32) i32 { return k + 1; };\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_27(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because you can't evaluate z(3) statically. */
  struct test_module a[] = { {
      "foo",
      "def x i32 = z(3);\n"
      "def z func[i32, i32] = fn(k i32) i32 { return k + 1; };\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_28(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def y i32 = -x;\n"
      "def x i32 = -3;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_29(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because of cyclic reference. */
  struct test_module a[] = { {
      "foo",
      "def y i32 = -x;\n"
      "def x i32 = -y;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_extern_1(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "extern putchar func[i32, i32];\n"
      "def foo func[i32] = fn()i32 {\n"
      "  putchar(65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_extern_2(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because putchar is called with the wrong type. */
  struct test_module a[] = { {
      "foo",
      "extern putchar func[i32, i32];\n"
      "def foo func[i32] = fn()i32 {\n"
      "  putchar(65u);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_extern_3(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because putchar has a nonsense return type. */
  struct test_module a[] = { {
      "foo",
      "extern putchar func[i32, quack];\n"
      "def foo func[i32] = fn()i32 {\n"
      "  putchar(65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_1(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar func[i32, i32] = foo;\n"
      "def foo func[i32, i32] = fn(x i32) i32 {\n"
      "  return x + 3;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_2(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar func[i32, i32] = foo;\n"
      "def[T] foo func[T, i32] = fn(x T) i32 {\n"
      "  return x + 3;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_3(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because foo's instatiation won't typecheck. */
  struct test_module a[] = { {
      "foo",
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar func[u32, i32] = foo;\n"
      "def[T] foo func[T, i32] = fn(x T) i32 {\n"
      "  return x + 3;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_4(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because foo lacks a return statement. */
  /* Unfortunately we don't check (yet) that _all_ paths return. */
  struct test_module a[] = { {
      "foo",
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  x + x;\n"
      "};"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_5(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo i32 = 7;\n"
      "def bar i32 = 5 << foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_6(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because shift overflows. */
  struct test_module a[] = { {
      "foo",
      "def foo i32 = 30;\n"
      "def bar i32 = 5 << foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_7(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] foo func[T, T] = fn(x T) T {\n"
      "  var y T = x;\n"
      "  return y;\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
      "  return foo(3);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_8(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] add32 func[i32, T, i32] = fn(x i32, y T) i32 {\n"
      "  var z i32 = convert(4);\n"
      "  return x + z;\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
      "  return add32(3, 4u);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_9(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because return type in return expression is wrong. */
  struct test_module a[] = { {
      "foo",
      "def foo func[i32] = fn() i32 {\n"
      "  return 4u;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}




int test_check_file(void) {
  int ret = 0;
  struct identmap im;
  identmap_init(&im);
  ident_value foo = identmap_intern_c_str(&im, "foo");

  DBG("test_check_file check_file_test_1...\n");
  if (!test_check_module(&im, &check_file_test_1, foo)) {
    DBG("check_file_test_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_2...\n");
  if (!test_check_module(&im, &check_file_test_2, foo)) {
    DBG("check_file_test_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_3...\n");
  if (!!test_check_module(&im, &check_file_test_3, foo)) {
    DBG("!check_file_test_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_4...\n");
  if (!test_check_module(&im, &check_file_test_4, foo)) {
    DBG("check_file_test_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_5...\n");
  if (!test_check_module(&im, &check_file_test_5, foo)) {
    DBG("check_file_test_5 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_6...\n");
  if (!test_check_module(&im, &check_file_test_6, foo)) {
    DBG("check_file_test_6 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_7...\n");
  if (!!test_check_module(&im, &check_file_test_7, foo)) {
    DBG("!check_file_test_7 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_8...\n");
  if (!test_check_module(&im, &check_file_test_8, foo)) {
    DBG("check_file_test_8 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_def_1...\n");
  if (!test_check_module(&im, &check_file_test_def_1, foo)) {
    DBG("check_file_test_def_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_def_2...\n");
  if (!!test_check_module(&im, &check_file_test_def_2, foo)) {
    DBG("check_file_test_def_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_def_3...\n");
  if (!test_check_module(&im, &check_file_test_def_3, foo)) {
    DBG("check_file_test_def_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_1...\n");
  if (!test_check_module(&im, &check_file_test_lambda_1, foo)) {
    DBG("check_file_test_lambda_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_2...\n");
  if (!test_check_module(&im, &check_file_test_lambda_2, foo)) {
    DBG("check_file_test_lambda_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_3...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_3, foo)) {
    DBG("check_file_test_lambda_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_4...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_4, foo)) {
    DBG("check_file_test_lambda_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_5...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_5, foo)) {
    DBG("check_file_test_lambda_5 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_6...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_6, foo)) {
    DBG("check_file_test_lambda_6 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_7...\n");
  if (!test_check_module(&im, &check_file_test_lambda_7, foo)) {
    DBG("check_file_test_lambda_7 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_8...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_8, foo)) {
    DBG("check_file_test_lambda_8 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_9...\n");
  if (!test_check_module(&im, &check_file_test_lambda_9, foo)) {
    DBG("check_file_test_lambda_9 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_10...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_10, foo)) {
    DBG("check_file_test_lambda_10 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_11...\n");
  if (!test_check_module(&im, &check_file_test_lambda_11, foo)) {
    DBG("check_file_test_lambda_11 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_12...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_12, foo)) {
    DBG("check_file_test_lambda_12 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_13...\n");
  if (!test_check_module(&im, &check_file_test_lambda_13, foo)) {
    DBG("check_file_test_lambda_13 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_14...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_14, foo)) {
    DBG("check_file_test_lambda_14 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_15...\n");
  if (!test_check_module(&im, &check_file_test_lambda_15, foo)) {
    DBG("check_file_test_lambda_15 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_16...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_16, foo)) {
    DBG("check_file_test_lambda_16 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_17...\n");
  if (!test_check_module(&im, &check_file_test_lambda_17, foo)) {
    DBG("check_file_test_lambda_17 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_18...\n");
  if (!test_check_module(&im, &check_file_test_lambda_18, foo)) {
    DBG("check_file_test_lambda_18 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_19...\n");
  if (!test_check_module(&im, &check_file_test_lambda_19, foo)) {
    DBG("check_file_test_lambda_19 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_20...\n");
  if (!test_check_module(&im, &check_file_test_lambda_20, foo)) {
    DBG("check_file_test_lambda_20 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_21...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_21, foo)) {
    DBG("check_file_test_lambda_21 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_22...\n");
  if (!test_check_module(&im, &check_file_test_lambda_22, foo)) {
    DBG("check_file_test_lambda_22 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_23...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_23, foo)) {
    DBG("check_file_test_lambda_23 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_24...\n");
  if (!test_check_module(&im, &check_file_test_lambda_24, foo)) {
    DBG("check_file_test_lambda_24 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_25...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_25, foo)) {
    DBG("check_file_test_lambda_25 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_26...\n");
  if (!test_check_module(&im, &check_file_test_lambda_26, foo)) {
    DBG("check_file_test_lambda_26 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_27...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_27, foo)) {
    DBG("check_file_test_lambda_27 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_28...\n");
  if (!test_check_module(&im, &check_file_test_lambda_28, foo)) {
    DBG("check_file_test_lambda_28 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_29...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_29, foo)) {
    DBG("check_file_test_lambda_29 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_extern_1...\n");
  if (!test_check_module(&im, &check_file_test_extern_1, foo)) {
    DBG("check_file_test_extern_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_extern_2...\n");
  if (!!test_check_module(&im, &check_file_test_extern_2, foo)) {
    DBG("check_file_test_extern_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_extern_3...\n");
  if (!!test_check_module(&im, &check_file_test_extern_3, foo)) {
    DBG("check_file_test_extern_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_1...\n");
  if (!test_check_module(&im, &check_file_test_more_1, foo)) {
    DBG("check_file_test_more_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_2...\n");
  if (!test_check_module(&im, &check_file_test_more_2, foo)) {
    DBG("check_file_test_more_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_3...\n");
  if (!!test_check_module(&im, &check_file_test_more_3, foo)) {
    DBG("check_file_test_more_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_4...\n");
  if (!!test_check_module(&im, &check_file_test_more_4, foo)) {
    DBG("check_file_test_more_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_5...\n");
  if (!test_check_module(&im, &check_file_test_more_5, foo)) {
    DBG("check_file_test_more_5 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_6...\n");
  if (!!test_check_module(&im, &check_file_test_more_6, foo)) {
    DBG("check_file_test_more_6 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_7...\n");
  if (!test_check_module(&im, &check_file_test_more_7, foo)) {
    DBG("check_file_test_more_7 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_8...\n");
  if (!test_check_module(&im, &check_file_test_more_8, foo)) {
    DBG("check_file_test_more_8 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_9...\n");
  if (!!test_check_module(&im, &check_file_test_more_9, foo)) {
    DBG("check_file_test_more_9 fails\n");
    goto cleanup_identmap;
  }

  ret = 1;
 cleanup_identmap:
  identmap_destroy(&im);
  return ret;
}
