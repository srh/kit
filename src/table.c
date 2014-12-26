#include "table.h"

#include <stdio.h>
#include <string.h>

#include "slice.h"
#include "typecheck.h"

void def_instantiation_init(struct def_instantiation *a,
                            struct ast_typeexpr **types,
                            size_t *types_count) {
  a->typecheck_started = 0;
  a->types = *types;
  a->types_count = *types_count;
  *types = NULL;
  *types_count = 0;
}

void def_instantiation_destroy(struct def_instantiation *a) {
  a->typecheck_started = 0;
  SLICE_FREE(a->types, a->types_count, ast_typeexpr_destroy);
}

void def_instantiation_free(struct def_instantiation **p) {
  def_instantiation_destroy(*p);
  free(*p);
  *p = NULL;
}

void def_entry_init(struct def_entry *e, ident_value name,
                    struct ast_generics *generics,
                    struct ast_typeexpr *type,
                    int is_primitive,
                    struct ast_def *def) {
  e->name = name;
  ast_generics_init_copy(&e->generics, generics);
  ast_typeexpr_init_copy(&e->type, type);
  e->is_primitive = is_primitive;
  e->def = def;

  e->instantiations = NULL;
  e->instantiations_count = 0;
  e->instantiations_limit = 0;

  e->static_references = NULL;
  e->static_references_count = 0;
  e->static_references_limit = 0;

  e->known_acyclic = 0;
  e->acyclicity_being_chased = 0;
}

void def_entry_destroy(struct def_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  ast_generics_destroy(&e->generics);
  ast_typeexpr_destroy(&e->type);
  e->is_primitive = 0;
  e->def = NULL;

  SLICE_FREE(e->instantiations, e->instantiations_count,
             def_instantiation_free);
  e->instantiations_limit = 0;
}

void def_entry_ptr_destroy(struct def_entry **ptr) {
  def_entry_destroy(*ptr);
  free(*ptr);
  *ptr = NULL;
}

void def_entry_note_static_reference(struct def_entry *ent,
                                     struct def_entry *reference) {
  for (size_t i = 0, e = ent->static_references_count; i < e; i++) {
    if (ent->static_references[i] == reference) {
      return;
    }
  }
  SLICE_PUSH(ent->static_references, ent->static_references_count,
             ent->static_references_limit, reference);
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
    int *heap_flatly_held = malloc_mul(flatly_held_count,
                                       sizeof(*heap_flatly_held));
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

int deftype_entry_param_is_flatly_held(struct deftype_entry *entry,
                                       size_t which_generic) {
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

  e->is_primitive = 0;
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

int name_table_help_add_def(struct name_table *t,
                            ident_value name,
                            struct ast_generics *generics,
                            struct ast_typeexpr *type,
                            int is_primitive,
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
  def_entry_init(new_entry, name, generics, type, is_primitive, def);
  SLICE_PUSH(t->defs, t->defs_count, t->defs_limit, new_entry);
  return 1;
}

int name_table_add_def(struct name_table *t,
                       ident_value name,
                       struct ast_generics *generics,
                       struct ast_typeexpr *type,
                       struct ast_def *def) {
  return name_table_help_add_def(t, name, generics, type, 0, def);
}

int name_table_add_primitive_def(struct name_table *t,
                                 ident_value name,
                                 struct ast_generics *generics,
                                 struct ast_typeexpr *type) {
  return name_table_help_add_def(t, name, generics, type, 1, NULL);
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
  deftype_entry_init_primitive(new_entry, name,
                               flatly_held, flatly_held_count);
  return name_table_help_add_deftype_entry(t, &new_entry);
}

void substitute_generics(struct ast_typeexpr *type,
                         struct ast_generics *g,
                         struct ast_typeexpr *args,
                         size_t args_count,
                         struct ast_typeexpr *concrete_type_out);

void substitute_generics_fields(struct ast_vardecl *fields,
                                size_t fields_count,
                                struct ast_generics *g,
                                struct ast_typeexpr *args,
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
    substitute_generics_fields(type->u.structe.fields,
                               type->u.structe.fields_count,
                               g, args, args_count,
                               &fields, &fields_count);

    ast_structe_init(&concrete_type_out->u.structe, ast_meta_make_garbage(),
                     fields, fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    concrete_type_out->tag = AST_TYPEEXPR_UNIONE;
    struct ast_vardecl *fields;
    size_t fields_count;
    substitute_generics_fields(type->u.unione.fields,
                               type->u.unione.fields_count,
                               g, args, args_count,
                               &fields, &fields_count);

    ast_unione_init(&concrete_type_out->u.unione, ast_meta_make_garbage(),
                    fields, fields_count);
  } break;
  default:
    UNREACHABLE();
  }
}

int combine_partial_types(struct ast_typeexpr *a,
                          struct ast_typeexpr *b,
                          struct ast_typeexpr *out);

int combine_fields_partial_types(struct ast_vardecl *a_fields,
                                 size_t a_fields_count,
                                 struct ast_vardecl *b_fields,
                                 size_t b_fields_count,
                                 struct ast_vardecl **fields_out,
                                 size_t *fields_count_out) {
  if (a_fields_count != b_fields_count) {
    return 0;
  }

  size_t fields_count = a_fields_count;
  struct ast_vardecl *fields = malloc_mul(sizeof(*fields), fields_count);
  for (size_t i = 0; i < fields_count; i++) {
    if (a_fields[i].name.value != b_fields[i].name.value) {
      goto fail;
    }
    struct ast_typeexpr type;
    if (!combine_partial_types(&a_fields[i].type, &b_fields[i].type,
                               &type)) {
      goto fail;
    }
    ast_vardecl_init(&fields[i], ast_meta_make_garbage(),
                     make_ast_ident(a_fields[i].name.value),
                     type);
    continue;
  fail:
    SLICE_FREE(fields, i, ast_vardecl_destroy);
    return 0;
  }

  *fields_out = fields;
  *fields_count_out = fields_count;
  return 1;
}

int combine_partial_types(struct ast_typeexpr *a,
                          struct ast_typeexpr *b,
                          struct ast_typeexpr *out) {
  if (a->tag == AST_TYPEEXPR_UNKNOWN) {
    ast_typeexpr_init_copy(out, b);
    return 1;
  }

  if (b->tag == AST_TYPEEXPR_UNKNOWN) {
    ast_typeexpr_init_copy(out, a);
    return 1;
  }

  if (a->tag != b->tag) {
    return 0;
  }

  switch (a->tag) {
  case AST_TYPEEXPR_NAME: {
    if (a->u.name.value != b->u.name.value) {
      return 0;
    }
    ast_typeexpr_init_copy(out, a);
    return 1;
  } break;
  case AST_TYPEEXPR_APP: {
    if (a->u.app.name.value != b->u.app.name.value
        || a->u.app.params_count != b->u.app.params_count) {
      return 0;
    }
    size_t params_count = a->u.app.params_count;
    struct ast_typeexpr *params = malloc_mul(sizeof(*params), params_count);
    for (size_t i = 0; i < params_count; i++) {
      if (!combine_partial_types(&a->u.app.params[i], &b->u.app.params[i],
                                 &params[i])) {
        SLICE_FREE(params, i, ast_typeexpr_destroy);
        return 0;
      }
    }
    out->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&out->u.app, ast_meta_make_garbage(),
                     make_ast_ident(a->u.app.name.value),
                     params, params_count);
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    struct ast_vardecl *fields;
    size_t fields_count;
    if (!combine_fields_partial_types(
            a->u.structe.fields, a->u.structe.fields_count,
            b->u.structe.fields, b->u.structe.fields_count,
            &fields, &fields_count)) {
      return 0;
    }
    out->tag = AST_TYPEEXPR_STRUCTE;
    ast_structe_init(&out->u.structe, ast_meta_make_garbage(),
                     fields, fields_count);
    return 1;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    struct ast_vardecl *fields;
    size_t fields_count;
    if (!combine_fields_partial_types(
            a->u.unione.fields, a->u.unione.fields_count,
            b->u.unione.fields, b->u.unione.fields_count,
            &fields, &fields_count)) {
      return 0;
    }
    out->tag = AST_TYPEEXPR_UNIONE;
    ast_unione_init(&out->u.unione, ast_meta_make_garbage(),
                    fields, fields_count);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int learn_materializations(struct ast_generics *g,
                           struct ast_typeexpr *materialized,
                           struct ast_typeexpr *type,
                           struct ast_typeexpr *partial_type);

int learn_fields_materializations(struct ast_generics *g,
                                  struct ast_typeexpr *materialized,
                                  struct ast_vardecl *fields,
                                  size_t fields_count,
                                  struct ast_vardecl *partial_fields,
                                  size_t partial_fields_count) {
  if (fields_count != partial_fields_count) {
    return 0;
  }

  for (size_t i = 0; i < fields_count; i++) {
    if (fields[i].name.value != partial_fields[i].name.value) {
      return 0;
    }
    if (!learn_materializations(g, materialized,
                                &fields[i].type,
                                &partial_fields[i].type)) {
      return 0;
    }
  }
  return 1;
}

int learn_materializations(struct ast_generics *g,
                           struct ast_typeexpr *materialized,
                           struct ast_typeexpr *type,
                           struct ast_typeexpr *partial_type) {
  CHECK(type->tag != AST_TYPEEXPR_UNKNOWN);
  if (partial_type->tag == AST_TYPEEXPR_UNKNOWN) {
    return 1;
  }
  size_t which_generic;
  if (type->tag == AST_TYPEEXPR_NAME
      && generics_lookup_name(g, type->u.name.value, &which_generic)) {
    struct ast_typeexpr combined;
    if (!combine_partial_types(&materialized[which_generic], partial_type,
                               &combined)) {
      return 0;
    }
    ast_typeexpr_destroy(&materialized[which_generic]);
    materialized[which_generic] = combined;
    return 1;
  }

  if (type->tag != partial_type->tag) {
    return 0;
  }

  switch (type->tag) {
  case AST_TYPEEXPR_NAME:
    return type->u.name.value == partial_type->u.name.value;
  case AST_TYPEEXPR_APP: {
    if (type->u.app.name.value != partial_type->u.app.name.value
        || type->u.app.params_count != partial_type->u.app.params_count) {
      return 0;
    }
    for (size_t i = 0, e = type->u.app.params_count; i < e; i++) {
      if (!learn_materializations(g, materialized,
                                  &type->u.app.params[i],
                                  &partial_type->u.app.params[i])) {
        return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    return learn_fields_materializations(g, materialized,
                                         type->u.structe.fields,
                                         type->u.structe.fields_count,
                                         partial_type->u.structe.fields,
                                         partial_type->u.structe.fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    return learn_fields_materializations(g, materialized,
                                         type->u.unione.fields,
                                         type->u.unione.fields_count,
                                         partial_type->u.unione.fields,
                                         partial_type->u.unione.fields_count);
  } break;
  default:
    UNREACHABLE();
  }
}

int is_concrete(struct ast_typeexpr *type);

int is_fields_concrete(struct ast_vardecl *fields, size_t fields_count) {
  for (size_t i = 0; i < fields_count; i++) {
    if (!is_concrete(&fields[i].type)) {
      return 0;
    }
  }
  return 1;
}

int is_concrete(struct ast_typeexpr *type) {
  if (type->tag == AST_TYPEEXPR_UNKNOWN) {
    return 0;
  }
  switch (type->tag) {
  case AST_TYPEEXPR_NAME:
    return 1;
  case AST_TYPEEXPR_APP: {
    for (size_t i = 0, e = type->u.app.params_count; i < e; i++) {
      if (!is_concrete(&type->u.app.params[i])) {
        return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return is_fields_concrete(type->u.structe.fields,
                              type->u.structe.fields_count);
  case AST_TYPEEXPR_UNIONE:
    return is_fields_concrete(type->u.unione.fields,
                              type->u.unione.fields_count);
  default:
    UNREACHABLE();
  }
}

int unify_with_parameterized_type(
    struct ast_generics *g,
    struct ast_typeexpr *type,
    struct ast_typeexpr *partial_type,
    struct ast_typeexpr **materialized_params_out,
    size_t *materialized_params_count_out,
    struct ast_typeexpr *concrete_type_out) {
  CHECK(g->has_type_params);
  size_t materialized_count = g->params_count;
  struct ast_typeexpr *materialized = malloc_mul(sizeof(*materialized),
                                                 materialized_count);
  for (size_t i = 0, e = g->params_count; i < e; i++) {
    materialized[i].tag = AST_TYPEEXPR_UNKNOWN;
  }

  if (!learn_materializations(g, materialized, type, partial_type)) {
    goto fail;
  }

  for (size_t i = 0, e = g->params_count; i < e; i++) {
    if (!is_concrete(&materialized[i])) {
      goto fail;
    }
  }

  substitute_generics(type, g, materialized, g->params_count, concrete_type_out);
  *materialized_params_out = materialized;
  *materialized_params_count_out = g->params_count;
  return 1;

 fail:
  SLICE_FREE(materialized, materialized_count, ast_typeexpr_destroy);
  return 0;
}

int typelists_equal(struct ast_typeexpr *a, size_t a_count,
                    struct ast_typeexpr *b, size_t b_count) {
  if (a_count != b_count) {
    return 0;
  }

  for (size_t i = 0; i < a_count; i++) {
    if (!exact_typeexprs_equal(a, b)) {
      return 0;
    }
  }

  return 1;
}

struct def_instantiation *def_entry_insert_instantiation(
    struct def_entry *ent,
    struct ast_typeexpr *materialized,
    size_t materialized_count) {
  CHECK(materialized_count == (ent->generics.has_type_params ?
                               ent->generics.params_count : 0));
  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations[i];
    if (typelists_equal(inst->types, inst->types_count,
                        materialized, materialized_count)) {
      return inst;
    }
  }

  size_t copy_count = materialized_count;
  struct ast_typeexpr *copy = malloc_mul(sizeof(*copy), copy_count);
  for (size_t i = 0; i < copy_count; i++) {
    ast_typeexpr_init_copy(&copy[i], &materialized[i]);
  }

  struct def_instantiation *inst = malloc(sizeof(*inst));
  CHECK(inst);
  def_instantiation_init(inst, &copy, &copy_count);
  SLICE_PUSH(ent->instantiations, ent->instantiations_count,
             ent->instantiations_limit, inst);
  return inst;
}

int def_entry_matches(struct def_entry *ent,
                      struct ast_typeexpr *generics_or_null,
                      size_t generics_count,
                      struct ast_typeexpr *partial_type,
                      struct ast_typeexpr *unified_type_out,
                      struct def_instantiation **instantiation_out) {
  if (!ent->generics.has_type_params) {
    if (generics_or_null) {
      return 0;
    }

    if (!unify_directionally(partial_type, &ent->type)) {
      return 0;
    }

    ast_typeexpr_init_copy(unified_type_out, &ent->type);
    *instantiation_out = def_entry_insert_instantiation(ent, NULL, 0);
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
    *instantiation_out = def_entry_insert_instantiation(ent,
                                                        generics_or_null,
                                                        generics_count);
  cleanup_concrete_type:
    ast_typeexpr_destroy(&ent_concrete_type);
    return ret;
  }

  /* The tricky case:  No generic parameters were given by the expression. */

  CHECK(ent->generics.has_type_params && !generics_or_null);

  struct ast_typeexpr *materialized_params;
  size_t materialized_params_count;
  struct ast_typeexpr concrete_type;
  if (!unify_with_parameterized_type(&ent->generics,
                                     &ent->type,
                                     partial_type,
                                     &materialized_params,
                                     &materialized_params_count,
                                     &concrete_type)) {
    return 0;
  }

  *unified_type_out = concrete_type;
  *instantiation_out
    = def_entry_insert_instantiation(ent,
                                     materialized_params,
                                     materialized_params_count);
  SLICE_FREE(materialized_params, materialized_params_count,
             ast_typeexpr_destroy);
  return 1;
}

int name_table_match_def(struct name_table *t,
                         ident_value name,
                         struct ast_typeexpr *generics_or_null,
                         size_t generics_count,
                         struct ast_typeexpr *partial_type,
                         struct ast_typeexpr *unified_type_out,
                         struct def_entry **entry_out,
                         struct def_instantiation **instantiation_out) {
  /* matched_type is initialized if matched_ent is non-null. */
  struct ast_typeexpr matched_type;
  struct def_instantiation *matched_instantiation = NULL;
  /* Get cl to shut up about the "uninitialized value". */
  memset(&matched_type, 0, sizeof(matched_type));
  struct def_entry *matched_ent = NULL;

  for (size_t i = 0, e = t->defs_count; i < e; i++) {
    struct def_entry *ent = t->defs[i];
    if (ent->name != name) {
      continue;
    }

    struct ast_typeexpr unified;
    struct def_instantiation *instantiation;
    if (def_entry_matches(ent, generics_or_null, generics_count,
                          partial_type, &unified, &instantiation)) {
      if (matched_ent) {
        ast_typeexpr_destroy(&unified);
        ERR_DBG("multiple matching definitions\n");
        goto fail_multiple_matching;
      } else {
        matched_type = unified;
        matched_instantiation = instantiation;
        matched_ent = ent;
      }
    }
  }

  if (!matched_ent) {
    ERR_DBG("no matching definition\n");
    goto fail;
  }

  *unified_type_out = matched_type;
  *entry_out = matched_ent;
  *instantiation_out = matched_instantiation;
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

