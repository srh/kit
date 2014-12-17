#include "table.h"

#include <stdio.h>
#include <string.h>

#include "slice.h"

void def_entry_destroy(struct def_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  ast_optional_type_params_destroy(&e->generics);
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

struct generics_arity params_arity(struct ast_optional_type_params *a) {
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
    e->flatly_held = malloc(size_mul(sizeof(*e->flatly_held), arity.value));
    CHECK(e->flatly_held);
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
    size_t bytes = size_mul(flatly_held_count, sizeof(*flatly_held));
    int *heap_flatly_held = malloc(bytes);
    CHECK(heap_flatly_held);
    memcpy(heap_flatly_held, flatly_held, bytes);
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


int generics_has_arity(struct ast_optional_type_params *generics,
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
		       struct ast_optional_type_params *generics,
		       struct ast_typeexpr *type,
		       struct ast_def *def) {
  if (deftype_shadowed(t, name)) {
    ERR_DBG("def name shadows deftype name.\n");
    return 0;
  }

  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    struct def_entry *ent = t->defs[i];
    if (ent->name != name) {
      continue;
    }

    /* TODO: This should be different, to allow overloading on
       different types. */
    if (!ent->generics.has_type_params || !generics->has_type_params) {
      ERR_DBG("untemplated def name clash.\n");
      return 0;
    }

    if (ent->generics.params_count == generics->params_count) {
      ERR_DBG("Templated defs have same arity.\n");
      return 0;
    }
  }

  struct def_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  new_entry->name = name;
  ast_optional_type_params_init_copy(&new_entry->generics, generics);
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

