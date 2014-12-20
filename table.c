#include "table.h"

#include <stdio.h>
#include <string.h>

#include "slice.h"
#include "typecheck.h"

void def_entry_destroy(struct def_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  ast_generics_destroy(&e->generics);
  ast_typeexpr_destroy(&e->type);
}

void def_entry_ptr_destroy(struct def_entry **ptr) {
  def_entry_destroy(*ptr);
  free(*ptr);
  *ptr = NULL;
}

struct generics_arity make_arity(size_t value) {
  struct generics_arity ret;
  ret.value = value;
  return ret;
}

struct generics_arity no_param_list_arity(void) {
  return make_arity(ARITY_NO_PARAMLIST);
}

struct generics_arity param_list_arity(size_t arity) {
  CHECK(arity != ARITY_NO_PARAMLIST);
  return make_arity(arity);
}

struct generics_arity params_arity(struct ast_generics *a) {
  return make_arity(a->has_type_params ? a->params_count : ARITY_NO_PARAMLIST);
}

int arity_no_paramlist(struct generics_arity arity) {
  return arity.value == ARITY_NO_PARAMLIST;
}

void deftype_entry_init(struct deftype_entry *e,
			ident_value name,
			struct generics_arity arity,
			struct ast_deftype *deftype) {
  e->name = name;
  e->arity = arity;

  if (arity_no_paramlist(arity)) {
    e->flatly_held = NULL;
    e->flatly_held_count = SIZE_MAX;
  } else {
    e->flatly_held = malloc_mul(sizeof(*e->flatly_held), arity.value);
    for (size_t i = 0, end = arity.value; i < end; i++) {
      e->flatly_held[i] = 0;
    }
    e->flatly_held_count = arity.value;
  }

  e->has_been_checked = 0;
  e->is_being_checked = 0;

  e->is_primitive = 0;
  e->deftype = deftype;
}

void deftype_entry_init_primitive(struct deftype_entry *e,
				  ident_value name,
				  int *flatly_held,
				  size_t flatly_held_count) {
  e->name = name;
  e->arity = flatly_held == NULL ?
    no_param_list_arity() : param_list_arity(flatly_held_count);

  if (flatly_held) {
    int *heap_flatly_held = malloc_mul(flatly_held_count, sizeof(*heap_flatly_held));
    memcpy(heap_flatly_held, flatly_held,
	   size_mul(flatly_held_count, sizeof(*heap_flatly_held)));
    e->flatly_held = heap_flatly_held;
    e->flatly_held_count = flatly_held_count;
  } else {
    e->flatly_held = NULL;
    e->flatly_held_count = 0;
  }

  e->has_been_checked = 1;
  e->is_being_checked = 0;

  e->is_primitive = 1;
  e->deftype = NULL;
}

int deftype_entry_param_is_flatly_held(struct deftype_entry *entry, size_t which_generic) {
  CHECK(entry->flatly_held);
  CHECK(which_generic < entry->flatly_held_count);
  return entry->flatly_held[which_generic];
}

void deftype_entry_destroy(struct deftype_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  e->arity.value = 0;

  free(e->flatly_held);
  e->flatly_held = NULL;
  e->flatly_held_count = 0;

  e->has_been_checked = 0;
  e->is_being_checked = 0;

  e->deftype = NULL;
}

void deftype_entry_ptr_destroy(struct deftype_entry **ptr) {
  deftype_entry_destroy(*ptr);
  free(*ptr);
  *ptr = NULL;
}

void name_table_init(struct name_table *t) {
  t->defs = NULL;
  t->defs_count = 0;
  t->defs_limit = 0;

  t->deftypes = NULL;
  t->deftypes_count = 0;
  t->deftypes_limit = 0;
}

void name_table_destroy(struct name_table *t) {
  SLICE_FREE(t->defs, t->defs_count, def_entry_ptr_destroy);
  SLICE_FREE(t->deftypes, t->deftypes_count, deftype_entry_ptr_destroy);
  name_table_init(t);
}


int generics_has_arity(struct ast_generics *generics,
		       struct generics_arity arity) {
  return generics->has_type_params
    ? generics->params_count == arity.value
    : arity_no_paramlist(arity);
}

int deftype_shadowed(struct name_table *t, ident_value name) {
  for (size_t i = 0, e = t->deftypes_count; i < e; i++) {
    if (t->deftypes[i]->name == name) {
      return 1;
    }
  }
  return 0;
}

int def_shadowed(struct name_table *t, ident_value name) {
  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    if (t->defs[i]->name == name) {
      return 1;
    }
  }
  return 0;
}

int name_table_shadowed(struct name_table *t, ident_value name) {
  return deftype_shadowed(t, name) || def_shadowed(t, name);
}

int name_table_add_def(struct name_table *t,
		       ident_value name,
		       struct ast_generics *generics,
		       struct ast_typeexpr *type,
		       struct ast_def *def) {
  if (deftype_shadowed(t, name)) {
    ERR_DBG("def name shadows deftype name.\n");
    return 0;
  }

  /* TODO: It would be nice to check for "obviously" conflicting defs
     here, instead of at overloading -- so we catch them without them
     needing to be used. */

  struct def_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  new_entry->name = name;
  ast_generics_init_copy(&new_entry->generics, generics);
  ast_typeexpr_init_copy(&new_entry->type, type);
  new_entry->def = def;
  SLICE_PUSH(t->defs, t->defs_count, t->defs_limit, new_entry);
  return 1;
}

int name_table_help_add_deftype_entry(struct name_table *t,
				      struct deftype_entry **entry_ptr) {
  struct deftype_entry *entry = *entry_ptr;
  *entry_ptr = NULL;

  if (def_shadowed(t, entry->name)) {
    ERR_DBG("deftype name shadows def name.\n");
    goto fail_cleanup_entry;
  }

  for (size_t i = 0, e = t->deftypes_count; i < e; i++) {
    struct deftype_entry *ent = t->deftypes[i];
    if (ent->name != entry->name) {
      continue;
    }

    if (arity_no_paramlist(ent->arity) || arity_no_paramlist(entry->arity)) {
      ERR_DBG("untemplated deftype name clash.\n");
      goto fail_cleanup_entry;
    }

    if (ent->arity.value == entry->arity.value) {
      ERR_DBG("templated deftypes have same arity.\n");
      goto fail_cleanup_entry;
    }
  }

  SLICE_PUSH(t->deftypes, t->deftypes_count, t->deftypes_limit, entry);
  return 1;

 fail_cleanup_entry:
  deftype_entry_ptr_destroy(&entry);
  return 0;
}

int name_table_add_deftype(struct name_table *t,
			   ident_value name,
			   struct generics_arity arity,
			   struct ast_deftype *deftype) {
  struct deftype_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  deftype_entry_init(new_entry, name, arity, deftype);
  return name_table_help_add_deftype_entry(t, &new_entry);
}

int name_table_add_primitive_type(struct name_table *t,
				  ident_value name,
				  int *flatly_held,
				  size_t flatly_held_count) {
  struct deftype_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  deftype_entry_init_primitive(new_entry, name, flatly_held, flatly_held_count);
  return name_table_help_add_deftype_entry(t, &new_entry);
}

int name_table_lookup_def(struct name_table *t,
			  ident_value name,
			  struct generics_arity arity,
			  struct def_entry **out) {
  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    struct def_entry *ent = t->defs[i];
    if (ent->name == name && generics_has_arity(&ent->generics, arity)) {
      *out = ent;
      return 1;
    }
  }
  return 0;
}

void substitute_generics(struct ast_typeexpr *type,
			 struct ast_generics *g,
			 struct ast_typeexpr *args,
			 size_t args_count,
			 struct ast_typeexpr *concrete_type_out);

void substitute_generics_fields(struct ast_vardecl *fields, size_t fields_count,
				struct ast_generics *g, struct ast_typeexpr *args,
				size_t args_count,
				struct ast_vardecl **concrete_fields_out,
				size_t *concrete_fields_count_out) {
  struct ast_vardecl *concrete_fields
    = malloc_mul(sizeof(*concrete_fields), fields_count);
  for (size_t i = 0; i < fields_count; i++) {
    struct ast_ident name;
    ast_ident_init_copy(&name, &fields[i].name);
    struct ast_typeexpr type;
    substitute_generics(&fields[i].type, g, args, args_count, &type);
    ast_vardecl_init(&concrete_fields[i], ast_meta_make_garbage(),
		     name, type);
  }

  *concrete_fields_out = concrete_fields;
  *concrete_fields_count_out = fields_count;
}

void substitute_generics(struct ast_typeexpr *type,
			 struct ast_generics *g,
			 struct ast_typeexpr *args,
			 size_t args_count,
			 struct ast_typeexpr *concrete_type_out) {
  CHECK(g->has_type_params);
  CHECK(g->params_count == args_count);

  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    size_t which_generic;
    if (generics_lookup_name(g, type->u.name.value, &which_generic)) {
      ast_typeexpr_init_copy(concrete_type_out, &args[which_generic]);
    } else {
      ast_typeexpr_init_copy(concrete_type_out, type);
    }
  } break;
  case AST_TYPEEXPR_APP: {
    concrete_type_out->tag = AST_TYPEEXPR_APP;
    size_t params_count = type->u.app.params_count;
    struct ast_typeexpr *params = malloc_mul(sizeof(*params), params_count);
    for (size_t i = 0; i < params_count; i++) {
      substitute_generics(&type->u.app.params[i], g, args, args_count,
			  &params[i]);
    }

    struct ast_ident name;
    ast_ident_init_copy(&name, &type->u.app.name);
    ast_typeapp_init(&concrete_type_out->u.app, ast_meta_make_garbage(),
		     name, params, params_count);
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    concrete_type_out->tag = AST_TYPEEXPR_STRUCTE;
    struct ast_vardecl *fields;
    size_t fields_count;
    substitute_generics_fields(type->u.structe.fields, type->u.structe.fields_count,
			       g, args, args_count,
			       &fields, &fields_count);

    ast_structe_init(&concrete_type_out->u.structe, ast_meta_make_garbage(),
		     fields, fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    concrete_type_out->tag = AST_TYPEEXPR_UNIONE;
    struct ast_vardecl *fields;
    size_t fields_count;
    substitute_generics_fields(type->u.unione.fields, type->u.unione.fields_count,
			       g, args, args_count,
			       &fields, &fields_count);

    ast_unione_init(&concrete_type_out->u.unione, ast_meta_make_garbage(),
		    fields, fields_count);
  } break;
  default:
    UNREACHABLE();
  }
}

int def_entry_matches(struct def_entry *ent,
		      struct ast_typeexpr *generics_or_null,
		      size_t generics_count,
		      struct ast_typeexpr *partial_type,
		      struct ast_typeexpr *unified_type_out) {
  if (!ent->generics.has_type_params) {
    if (generics_or_null) {
      return 0;
    }

    if (!unify_directionally(partial_type, &ent->type)) {
      return 0;
    }

    ast_typeexpr_init_copy(unified_type_out, &ent->type);
    return 1;
  }

  if (generics_or_null) {
    if (ent->generics.params_count != generics_count) {
      return 0;
    }

    struct ast_typeexpr ent_concrete_type;
    substitute_generics(&ent->type, &ent->generics,
			generics_or_null, generics_count,
			&ent_concrete_type);

    int ret = 0;
    if (!unify_directionally(partial_type, &ent_concrete_type)) {
      goto cleanup_concrete_type;
    }

    ret = 1;
    ast_typeexpr_init_copy(unified_type_out, &ent_concrete_type);
  cleanup_concrete_type:
    ast_typeexpr_destroy(&ent_concrete_type);
    return ret;
  }

  /* The tricky case:  No generic parameters were given by the expression. */
  /* TODO: Implement. */
  return 0;
}

int name_table_match_def(struct name_table *t,
			 ident_value name,
			 struct ast_typeexpr *generics_or_null,
			 size_t generics_count,
			 struct ast_typeexpr *partial_type,
			 struct ast_typeexpr *unified_type_out,
			 struct def_entry **entry_out) {
  /* matched_type is initialized if matched_ent is non-null. */
  struct ast_typeexpr matched_type;
  /* Get cl to shut up about the "uninitialized value". */
  memset(&matched_type, 0, sizeof(matched_type));
  struct def_entry *matched_ent = NULL;

  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    struct def_entry *ent = t->defs[i];
    if (ent->name != name) {
      continue;
    }

    struct ast_typeexpr unified;
    if (def_entry_matches(ent, generics_or_null, generics_count,
			  partial_type, &unified)) {
      if (matched_ent) {
	ast_typeexpr_destroy(&unified);
	ERR_DBG("multiple matching definitions\n");
	goto fail_multiple_matching;
      } else {
	matched_type = unified;
	matched_ent = ent;
      }
    }
  }

  if (!matched_ent) {
    goto fail;
  }

  *unified_type_out = matched_type;
  *entry_out = matched_ent;
  return 1;

 fail_multiple_matching:
  ast_typeexpr_destroy(&matched_type);
 fail:
  return 0;
}

int name_table_lookup_deftype(struct name_table *t,
			      ident_value name,
			      struct generics_arity arity,
			      struct deftype_entry **out) {
  for (size_t i = 0, e = t->deftypes_count; i < e; i++) {
    struct deftype_entry *ent = t->deftypes[i];
    if (ent->name == name && ent->arity.value == arity.value) {
      *out = ent;
      return 1;
    }
  }
  return 0;
}

struct deftype_entry *lookup_deftype(struct name_table *t,
				     struct ast_deftype *a) {
  struct deftype_entry *ent;
  int res = name_table_lookup_deftype(t, a->name.value,
				      params_arity(&a->generics),
				      &ent);
  CHECK(res);
  return ent;
}

int deftype_has_been_checked(struct name_table *t,
			     struct ast_deftype *a) {
  return lookup_deftype(t, a)->has_been_checked;
}

int deftype_is_being_checked(struct name_table *t,
			     struct ast_deftype *a) {
  return lookup_deftype(t, a)->is_being_checked;
}

void deftype_entry_mark_is_being_checked(struct deftype_entry *ent) {
  CHECK(!ent->is_being_checked);
  CHECK(!ent->has_been_checked);
  ent->is_being_checked = 1;
}

void deftype_entry_mark_has_been_checked(struct deftype_entry *ent) {
  CHECK(!ent->has_been_checked);
  CHECK(ent->is_being_checked);
  ent->is_being_checked = 0;
  ent->has_been_checked = 1;
}

void deftype_entry_mark_generic_flatly_held(struct deftype_entry *ent,
					    size_t which_generic) {
  CHECK(ent->is_being_checked);
  CHECK(ent->flatly_held);
  CHECK(which_generic < ent->flatly_held_count);
  ent->flatly_held[which_generic] = 1;
}

