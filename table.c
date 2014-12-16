#include "table.h"

#include <stdio.h>

#include "slice.h"

void def_entry_destroy(struct def_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  ast_optional_type_params_destroy(&e->generics);
  ast_typeexpr_destroy(&e->type);
}

void deftype_entry_destroy(struct deftype_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  e->arity.value = 0;
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
  SLICE_FREE(t->defs, t->defs_count, def_entry_destroy);
  SLICE_FREE(t->deftypes, t->deftypes_count, deftype_entry_destroy);
  name_table_init(t);
}

int arity_no_paramlist(struct generics_arity arity) {
  return arity.value == SIZE_MAX;
}

int generics_has_arity(struct ast_optional_type_params *generics,
		       struct generics_arity arity) {
  return generics->has_type_params
    ? generics->params_count == arity.value
    : arity_no_paramlist(arity);
}

int deftype_shadowed(struct name_table *t, ident_value name) {
  for (size_t i = 0, e = t->deftypes_count; i < e; i++) {
    if (t->deftypes[i].name == name) {
      return 1;
    }
  }
  return 0;
}

int def_shadowed(struct name_table *t, ident_value name) {
  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    if (t->defs[i].name == name) {
      return 1;
    }
  }
  return 0;
}

int name_table_add_def(struct name_table *t,
		       ident_value name,
		       struct ast_optional_type_params *generics,
		       struct ast_typeexpr *type) {
  if (deftype_shadowed(t, name)) {
    ERR_DBG("def name shadows deftype name.\n");
    return 0;
  }

  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    struct def_entry *ent = &t->defs[i];
    if (ent->name != name) {
      continue;
    }

    if (!ent->generics.has_type_params || !generics->has_type_params) {
      ERR_DBG("untemplated def name clash.\n");
      return 0;
    }

    if (ent->generics.params_count == generics->params_count) {
      ERR_DBG("Templated defs have same arity.\n");
      return 0;
    }
  }

  struct def_entry new_entry;
  new_entry.name = name;
  ast_optional_type_params_init_copy(&new_entry.generics, generics);
  ast_typeexpr_init_copy(&new_entry.type, type);
  SLICE_PUSH(t->defs, t->defs_count, t->defs_limit, new_entry);
  return 1;
}

int name_table_add_deftype(struct name_table *t,
			   ident_value name,
			   struct generics_arity arity) {
  if (def_shadowed(t, name)) {
    ERR_DBG("deftype name shadows def name.\n");
    return 0;
  }

  for (size_t i = 0, e = t->deftypes_count; i < e; i++) {
    struct deftype_entry *ent = &t->deftypes[i];
    if (ent->name != name) {
      continue;
    }

    if (arity_no_paramlist(ent->arity) || arity_no_paramlist(arity)) {
      ERR_DBG("untemplated deftype name clash.\n");
      return 0;
    }

    if (ent->arity.value == arity.value) {
      ERR_DBG("templated deftypes have same arity.\n");
      return 0;
    }
  }

  struct deftype_entry new_entry;
  new_entry.name = name;
  new_entry.arity = arity;
  SLICE_PUSH(t->deftypes, t->deftypes_count, t->deftypes_limit, new_entry);
  return 1;
}

int name_table_lookup_def(struct name_table *t,
			  ident_value name,
			  struct generics_arity arity,
			  struct def_entry **out) {
  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    struct def_entry *ent = &t->defs[i];
    if (ent->name == name && generics_has_arity(&ent->generics, arity)) {
      *out = ent;
      return 1;
    }
  }
  return 0;
}

int name_table_lookup_deftype(struct name_table *t,
			      ident_value name,
			      struct generics_arity arity,
			      struct deftype_entry **out) {
  for (size_t i = 0, e = t->deftypes_count; i < e; i++) {
    struct deftype_entry *ent = &t->deftypes[i];
    if (ent->name == name && ent->arity.value == arity.value) {
      *out = ent;
      return 1;
    }
  }
  return 0;
}
