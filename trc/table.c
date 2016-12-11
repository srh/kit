#include "table.h"

#include <stdio.h>
#include <string.h>

#include "slice.h"
#include "print.h"
#include "typecheck.h"

GEN_SLICE_IMPL(def_entry_ptr, struct def_entry *);
GEN_SLICE_IMPL(def_entry_nonowning_ptr, struct def_entry *);
GEN_SLICE_IMPL(deftype_entry_ptr, struct deftype_entry *);
GEN_SLICE_IMPL(def_instantiation_ptr, struct def_instantiation *);
GEN_SLICE_IMPL(deftype_instantiation_ptr, struct deftype_instantiation *);
GEN_SLICE_IMPL(defclass_ident, struct defclass_ident);

struct defs_by_name_node {
  struct def_entry *ent;
  struct defs_by_name_node *next;
};

struct deftypes_by_name_node {
  struct deftype_entry *ent;
  struct deftypes_by_name_node *next;
};

void def_instantiation_init(struct def_instantiation *a,
                            struct def_entry *owner,
                            struct ast_typeexpr **substitutions,
                            size_t *substitutions_count,
                            struct ast_typeexpr *concrete_type) {
  a->owner = owner;
  a->typecheck_started = 0;
  a->substitutions = *substitutions;
  a->substitutions_count = *substitutions_count;
  ast_typeexpr_init_copy(&a->type, concrete_type);
  a->annotated_rhs_computed = 0;
  a->value_computed = 0;
  a->symbol_table_index_computed = 0;
  *substitutions = NULL;
  *substitutions_count = 0;
}

void def_instantiation_destroy(struct def_instantiation *a) {
  a->owner = NULL;
  a->typecheck_started = 0;
  SLICE_FREE(a->substitutions, a->substitutions_count, ast_typeexpr_destroy);
  ast_typeexpr_destroy(&a->type);
  if (a->annotated_rhs_computed) {
    ast_expr_destroy(&a->annotated_rhs);
    a->annotated_rhs_computed = 0;
  }
  if (a->value_computed) {
    static_value_destroy(&a->value);
    a->value_computed = 0;
  }
}

void def_instantiation_free(struct def_instantiation **p) {
  def_instantiation_destroy(*p);
  free(*p);
  *p = NULL;
}

struct ast_expr *di_annotated_rhs(struct def_instantiation *inst) {
  CHECK(inst->annotated_rhs_computed);
  return &inst->annotated_rhs;
}
void di_set_annotated_rhs(struct def_instantiation *inst,
                          struct ast_expr annotated_rhs) {
  CHECK(!inst->annotated_rhs_computed);
  inst->annotated_rhs_computed = 1;
  inst->annotated_rhs = annotated_rhs;
}
struct static_value *di_value(struct def_instantiation *inst) {
  CHECK(inst->value_computed);
  return &inst->value;
}
struct static_value *di_value_for_set(struct def_instantiation *inst) {
  CHECK(!inst->value_computed);
  inst->value_computed = 1;
  return &inst->value;
}
struct sti di_symbol_table_index(struct def_instantiation *inst) {
  CHECK(inst->symbol_table_index_computed);
  return inst->symbol_table_index;
}
void di_set_symbol_table_index(struct def_instantiation *inst,
                               struct sti symbol_table_index) {
  CHECK(!inst->symbol_table_index_computed);
  inst->symbol_table_index_computed = 1;
  inst->symbol_table_index = symbol_table_index;
}

void defclass_ident_init_copy(struct defclass_ident *a, struct defclass_ident *c) {
  *a = *c;
}

void def_entry_init(struct def_entry *e, ident_value name,
                    struct ast_generics *generics,
                    struct ast_typeexpr *type,
                    struct defclass_ident *accessible,
                    size_t accessible_count,
                    struct defclass_ident *private_to,
                    size_t private_to_count,
                    int is_primitive,
                    struct primitive_op primitive_op,
                    int is_extern,
                    int is_export,
                    struct ast_def *def) {
  e->name = name;
  ast_generics_init_copy(&e->generics, generics);
  ast_typeexpr_init_copy(&e->type, type);

  SLICE_INIT_COPY(e->accessible, e->accessible_count,
                  accessible, accessible_count,
                  defclass_ident_init_copy);

  SLICE_INIT_COPY(e->private_to, e->private_to_count,
                  private_to, private_to_count,
                  defclass_ident_init_copy);

  e->is_primitive = is_primitive;
  e->primitive_op = primitive_op;
  e->is_extern = is_extern;
  e->is_export = is_export;
  e->def = def;

  e->instantiations = def_instantiation_ptr_slice_initializer();

  e->static_references = def_entry_nonowning_ptr_slice_initializer();

  e->known_acyclic = 0;
  e->acyclicity_being_chased = 0;
}

void def_entry_destroy(struct def_entry *e) {
  e->name = IDENT_VALUE_INVALID;
  ast_generics_destroy(&e->generics);
  ast_typeexpr_destroy(&e->type);

  free(e->accessible);
  e->accessible = NULL;
  e->accessible_count = 0;

  free(e->private_to);
  e->private_to = NULL;
  e->private_to_count = 0;

  e->is_primitive = 0;
  e->primitive_op = make_primop(PRIMITIVE_OP_INVALID);
  e->is_extern = 0;
  e->def = NULL;

  def_instantiation_ptr_slice_destroy(&e->instantiations,
                                      def_instantiation_free);

  def_entry_nonowning_ptr_slice_destroy_prim(&e->static_references);

  e->known_acyclic = 0;
  e->acyclicity_being_chased = 0;
}

void def_entry_ptr_destroy(struct def_entry **ptr) {
  def_entry_destroy(*ptr);
  free(*ptr);
  *ptr = NULL;
}

void def_entry_note_static_reference(struct def_entry *ent,
                                     struct def_entry *reference) {
  for (size_t i = 0, e = ent->static_references.count; i < e; i++) {
    if (ent->static_references.ptr[i] == reference) {
      return;
    }
  }
  def_entry_nonowning_ptr_slice_push(&ent->static_references, reference);
}

void deftype_instantiation_init(struct deftype_instantiation *inst,
                                struct ast_typeexpr *substs, size_t substs_count) {
  inst->substitutions = substs;
  inst->substitutions_count = substs_count;
  inst->has_typeexpr_traits = 0;
}

void deftype_instantiation_destroy(struct deftype_instantiation *inst) {
  SLICE_FREE(inst->substitutions, inst->substitutions_count, ast_typeexpr_destroy);
  if (inst->has_typeexpr_traits) {
    ast_deftype_rhs_destroy(&inst->concrete_rhs);
    inst->has_typeexpr_traits = 0;
  }
}

void deftype_instantiation_free(struct deftype_instantiation **p) {
  deftype_instantiation_destroy(*p);
  free(*p);
  *p = NULL;
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

  e->instantiations = deftype_instantiation_ptr_slice_initializer();

  e->has_been_checked = 0;
  e->is_being_checked = 0;

  e->is_primitive = 0;
  e->primitive_sizeof = 0;
  e->primitive_alignof = 0;
  e->deftype = deftype;
}

void deftype_entry_init_primitive(struct deftype_entry *e,
                                  ident_value name,
                                  int *flatly_held,
                                  size_t flatly_held_count,
                                  uint32_t primitive_sizeof,
                                  uint32_t primitive_alignof) {
  e->name = name;
  e->arity = flatly_held == NULL ?
    no_param_list_arity() : param_list_arity(flatly_held_count);

  if (flatly_held) {
    int *heap_flatly_held = malloc_mul(flatly_held_count,
                                       sizeof(*heap_flatly_held));
    ok_memcpy(heap_flatly_held, flatly_held,
              size_mul(flatly_held_count, sizeof(*heap_flatly_held)));
    e->flatly_held = heap_flatly_held;
    e->flatly_held_count = flatly_held_count;
  } else {
    e->flatly_held = NULL;
    e->flatly_held_count = 0;
  }

  e->instantiations = deftype_instantiation_ptr_slice_initializer();

  e->has_been_checked = 1;
  e->is_being_checked = 0;

  e->is_primitive = 1;
  e->primitive_sizeof = primitive_sizeof;
  e->primitive_alignof = primitive_alignof;
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

  deftype_instantiation_ptr_slice_destroy(&e->instantiations,
                                          deftype_instantiation_free);

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

void name_table_init(struct name_table *t, enum target_arch arch) {
  arena_init(&t->arena);

  t->defs = def_entry_ptr_slice_initializer();

  identmap_init(&t->defs_by_name);

  t->deftypes = deftype_entry_ptr_slice_initializer();

  identmap_init(&t->deftypes_by_name);

  t->arch = arch;
}

void name_table_destroy(struct name_table *t) {
  t->arch = (enum target_arch)-1;

  identmap_destroy(&t->deftypes_by_name);

  deftype_entry_ptr_slice_destroy(&t->deftypes, deftype_entry_ptr_destroy);

  identmap_destroy(&t->defs_by_name);

  def_entry_ptr_slice_destroy(&t->defs, def_entry_ptr_destroy);

  arena_destroy(&t->arena);
}


int generics_has_arity(struct ast_generics *generics,
                       struct generics_arity arity) {
  return generics->has_type_params
    ? generics->params_count == arity.value
    : arity_no_paramlist(arity);
}

int deftype_shadowed(struct name_table *t, ident_value name) {
  ident_value dtbn_id;
  return identmap_is_interned(&t->deftypes_by_name, &name, sizeof(name),
                              &dtbn_id);
}

int def_shadowed(struct name_table *t, ident_value name) {
  ident_value dbn_id;
  return identmap_is_interned(&t->defs_by_name, &name, sizeof(name),
                              &dbn_id);
}

int name_table_shadowed(struct name_table *t, ident_value name) {
  return deftype_shadowed(t, name) || def_shadowed(t, name);
}

int name_table_help_add_def(struct identmap *im,
                            struct name_table *t,
                            ident_value name,
                            struct ast_generics *generics,
                            struct ast_typeexpr *type,
                            struct defclass_ident *accessible,
                            size_t accessible_count,
                            struct defclass_ident *private_to,
                            size_t private_to_count,
                            int is_primitive,
                            struct primitive_op primitive_op,
                            int is_extern,
                            int is_export,
                            struct ast_def *def) {
  if (deftype_shadowed(t, name)) {
    ERR_DBG("def name '%.*s' shadows deftype name.\n", IM_P(im, name));
    return 0;
  }

  if (is_extern || is_export) {
    if (def_shadowed(t, name)) {
      ERR_DBG("extern or exported def name '%.*s' shadows fellow def name.\n",
              IM_P(im, name));
      return 0;
    }
  } else {
    ident_value dbn_id;
    if (identmap_is_interned(&t->defs_by_name, &name, sizeof(name),
                             &dbn_id)) {
      struct defs_by_name_node *node
        = identmap_get_user_value(&t->defs_by_name, dbn_id);
      CHECK(node);
      if (node->ent->is_extern || node->ent->is_export) {
        ERR_DBG("def name '%.*s' shadows extern or exported def name.\n",
                IM_P(im, name));
        return 0;
      }
      /* (We only need to check the first node, because extern/export
      defs disallow there to be conflicting names.) */
    }
  }

  struct def_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  def_entry_init(new_entry, name, generics, type,
                 accessible, accessible_count,
                 private_to, private_to_count,
                 is_primitive, primitive_op,
                 is_extern, is_export, def);
  def_entry_ptr_slice_push(&t->defs, new_entry);

  ident_value dbn_id = identmap_intern(&t->defs_by_name, &name, sizeof(name));
  struct defs_by_name_node *node = ARENA_TYPED(&t->arena,
                                               struct defs_by_name_node);
  node->next = identmap_get_user_value(&t->defs_by_name, dbn_id);
  node->ent = new_entry;
  identmap_set_user_value(&t->defs_by_name, dbn_id, node);
  return 1;
}

int name_table_add_def(struct identmap *im,
                       struct name_table *t,
                       ident_value name,
                       struct ast_generics *generics,
                       struct ast_typeexpr *type,
                       struct defclass_ident *accessible,
                       size_t accessible_count,
                       int is_export,
                       struct ast_def *def) {
  CHECK(!(is_export && generics->has_type_params));
  return name_table_help_add_def(im, t, name, generics, type,
                                 accessible, accessible_count,
                                 NULL, 0,  /* Not private to anything. */
                                 0, make_primop(PRIMITIVE_OP_INVALID),
                                 0, is_export, def);
}

int name_table_add_private_primitive_def(struct identmap *im,
                                         struct name_table *t,
                                         ident_value name,
                                         struct primitive_op primitive_op,
                                         struct ast_generics *generics,
                                         struct ast_typeexpr *type,
                                         struct defclass_ident *private_to,
                                         size_t private_to_count) {
  return name_table_help_add_def(im, t, name, generics, type,
                                 NULL, 0, /* Needs no special access. */
                                 private_to, private_to_count,
                                 1, primitive_op,
                                 0, 0, NULL);
}

int name_table_add_primitive_def(struct identmap *im,
                                 struct name_table *t,
                                 ident_value name,
                                 struct primitive_op primitive_op,
                                 struct ast_generics *generics,
                                 struct ast_typeexpr *type) {
  return name_table_help_add_def(im, t, name, generics, type,
                                 NULL, 0, /* Needs no special access. */
                                 NULL, 0, /* Not private to anything. */
                                 1, primitive_op,
                                 0, 0, NULL);
}

int name_table_add_extern_def(struct identmap *im,
                              struct name_table *t,
                              ident_value name,
                              struct ast_typeexpr *type) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  return name_table_help_add_def(im, t, name, &generics, type,
                                 NULL, 0, /* Needs no special access. */
                                 NULL, 0, /* Not private to anything. */
                                 0, make_primop(PRIMITIVE_OP_INVALID),
                                 1, 0, NULL);
}

int name_table_help_add_deftype_entry(struct identmap *im,
                                      struct name_table *t,
                                      struct deftype_entry **entry_ptr) {
  struct deftype_entry *entry = *entry_ptr;
  ident_value name = entry->name;
  *entry_ptr = NULL;

  if (def_shadowed(t, name)) {
    ERR_DBG("deftype name '%.*s' shadows def name.\n", IM_P(im, name));
    goto fail_cleanup_entry;
  }

  ident_value dtbn_id = identmap_intern(&t->deftypes_by_name,
                                        &name, sizeof(name));
  struct deftypes_by_name_node *old_node
    = identmap_get_user_value(&t->deftypes_by_name, dtbn_id);

  for (struct deftypes_by_name_node *node = old_node;
       node;
       node = node->next) {
    struct deftype_entry *ent = node->ent;
    CHECK(ent->name == name);

    if (arity_no_paramlist(ent->arity) || arity_no_paramlist(entry->arity)) {
      ERR_DBG("untemplated deftype name clash for '%.*s'.\n", IM_P(im, name));
      goto fail_cleanup_entry;
    }

    if (ent->arity.value == entry->arity.value) {
      ERR_DBG("templated deftypes for '%.*s' have same arity.\n", IM_P(im, name));
      goto fail_cleanup_entry;
    }
  }

  deftype_entry_ptr_slice_push(&t->deftypes, entry);
  struct deftypes_by_name_node *new_node
    = ARENA_TYPED(&t->arena, struct deftypes_by_name_node);
  new_node->next = old_node;
  new_node->ent = entry;
  identmap_set_user_value(&t->deftypes_by_name, dtbn_id, new_node);
  return 1;

 fail_cleanup_entry:
  deftype_entry_ptr_destroy(&entry);
  return 0;
}

int name_table_add_deftype(struct identmap *im,
                           struct name_table *t,
                           ident_value name,
                           struct generics_arity arity,
                           struct ast_deftype *deftype) {
  struct deftype_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  deftype_entry_init(new_entry, name, arity, deftype);
  return name_table_help_add_deftype_entry(im, t, &new_entry);
}

int name_table_add_primitive_type(struct identmap *im,
                                  struct name_table *t,
                                  ident_value name,
                                  int *flatly_held,
                                  size_t flatly_held_count,
                                  uint32_t primitive_sizeof,
                                  uint32_t primitive_alignof) {
  struct deftype_entry *new_entry = malloc(sizeof(*new_entry));
  CHECK(new_entry);
  deftype_entry_init_primitive(new_entry, name,
                               flatly_held, flatly_held_count,
                               primitive_sizeof, primitive_alignof);
  return name_table_help_add_deftype_entry(im, t, &new_entry);
}

int combine_partial_types(struct identmap *im,
                          struct ast_typeexpr *a,
                          struct ast_typeexpr *b,
                          struct ast_typeexpr *out);

int combine_fields_partial_types(struct identmap *im,
                                 struct ast_vardecl *a_fields,
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
    if (!combine_partial_types(im, &a_fields[i].type, &b_fields[i].type,
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

int is_numeric_type(struct identmap *im, struct ast_typeexpr *a) {
  if (a->tag != AST_TYPEEXPR_NAME) {
    return 0;
  }

  ident_value val = a->u.name.value;
  const char *names[8] = { U8_TYPE_NAME, I8_TYPE_NAME, U16_TYPE_NAME,
                           I16_TYPE_NAME, U32_TYPE_NAME, I32_TYPE_NAME,
                           SIZE_TYPE_NAME, OSIZE_TYPE_NAME };
  for (size_t i = 0; i < (sizeof(names) / sizeof(names[0])); i++) {
    if (val == identmap_intern_c_str(im, names[i])) {
      return 1;
    }
  }
  return 0;
}

int combine_partial_types(struct identmap *im,
                          struct ast_typeexpr *a,
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

  if (a->tag == AST_TYPEEXPR_NUMERIC) {
    if (b->tag == AST_TYPEEXPR_NUMERIC || is_numeric_type(im, b)) {
      ast_typeexpr_init_copy(out, b);
      return 1;
    } else {
      return 0;
    }
  }

  if (b->tag == AST_TYPEEXPR_NUMERIC) {
    if (is_numeric_type(im, a)) {
      ast_typeexpr_init_copy(out, a);
      return 1;
    } else {
      return 0;
    }
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
      if (!combine_partial_types(im, &a->u.app.params[i], &b->u.app.params[i],
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
            im,
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
            im,
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
  case AST_TYPEEXPR_ARRAY: {
    uint32_t a_count = unsafe_numeric_literal_u32(&a->u.arraytype.number);
    {
      uint32_t b_count = unsafe_numeric_literal_u32(&b->u.arraytype.number);
      if (a_count != b_count) {
        return 0;
      }
    }
    struct ast_typeexpr param;
    if (!combine_partial_types(im, a->u.arraytype.param, b->u.arraytype.param, &param)) {
      return 0;
    }
    struct ast_numeric_literal a_number;
    ast_numeric_literal_init_copy(&a_number, &a->u.arraytype.number);
    out->tag = AST_TYPEEXPR_ARRAY;
    ast_arraytype_init(&out->u.arraytype, ast_meta_make_garbage(),
                       a_number, param);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int learn_materializations(struct identmap *im,
                           struct ast_generics *g,
                           struct ast_typeexpr *materialized,
                           struct ast_typeexpr *type,
                           struct ast_typeexpr *partial_type);

int learn_fields_materializations(struct identmap *im,
                                  struct ast_generics *g,
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
    if (!learn_materializations(im, g, materialized,
                                &fields[i].type,
                                &partial_fields[i].type)) {
      return 0;
    }
  }
  return 1;
}

/* type is a complete type with generics in it, partial_type is a
concrete partial type, generics/materialized is a mapping from generic
names to concrete partial types. */
int learn_materializations(struct identmap *im,
                           struct ast_generics *g,
                           struct ast_typeexpr *materialized,
                           struct ast_typeexpr *type,
                           struct ast_typeexpr *partial_type) {
  CHECK(type->tag != AST_TYPEEXPR_UNKNOWN && type->tag != AST_TYPEEXPR_NUMERIC);
  if (partial_type->tag == AST_TYPEEXPR_UNKNOWN) {
    return 1;
  }
  size_t which_generic;
  if (type->tag == AST_TYPEEXPR_NAME
      && generics_lookup_name(g, type->u.name.value, &which_generic)) {
    struct ast_typeexpr combined;
    if (!combine_partial_types(im, &materialized[which_generic], partial_type,
                               &combined)) {
      return 0;
    }
    ast_typeexpr_destroy(&materialized[which_generic]);
    materialized[which_generic] = combined;
    return 1;
  }

  if (partial_type->tag == AST_TYPEEXPR_NUMERIC) {
    return is_numeric_type(im, type);
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
      if (!learn_materializations(im, g, materialized,
                                  &type->u.app.params[i],
                                  &partial_type->u.app.params[i])) {
        return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    return learn_fields_materializations(im, g, materialized,
                                         type->u.structe.fields,
                                         type->u.structe.fields_count,
                                         partial_type->u.structe.fields,
                                         partial_type->u.structe.fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    return learn_fields_materializations(im, g, materialized,
                                         type->u.unione.fields,
                                         type->u.unione.fields_count,
                                         partial_type->u.unione.fields,
                                         partial_type->u.unione.fields_count);
  } break;
  case AST_TYPEEXPR_ARRAY: {
    {
      uint32_t type_count = unsafe_numeric_literal_u32(&type->u.arraytype.number);
      uint32_t pt_count = unsafe_numeric_literal_u32(&partial_type->u.arraytype.number);
      if (type_count != pt_count) {
        return 0;
      }
    }

    return learn_materializations(im, g, materialized, type->u.arraytype.param,
                                  partial_type->u.arraytype.param);
  } break;
  default:
    UNREACHABLE();
  }
}

int is_fields_complete(struct ast_vardecl *fields, size_t fields_count) {
  for (size_t i = 0; i < fields_count; i++) {
    if (!is_complete(&fields[i].type)) {
      return 0;
    }
  }
  return 1;
}

int is_complete(struct ast_typeexpr *type) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME:
    return 1;
  case AST_TYPEEXPR_APP: {
    for (size_t i = 0, e = type->u.app.params_count; i < e; i++) {
      if (!is_complete(&type->u.app.params[i])) {
        return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return is_fields_complete(type->u.structe.fields,
                              type->u.structe.fields_count);
  case AST_TYPEEXPR_UNIONE:
    return is_fields_complete(type->u.unione.fields,
                              type->u.unione.fields_count);
  case AST_TYPEEXPR_ARRAY:
    return is_complete(type->u.arraytype.param);
  case AST_TYPEEXPR_UNKNOWN:
    return 0;
  case AST_TYPEEXPR_NUMERIC:
    return 0;
  default:
    UNREACHABLE();
  }
}

enum match_result unify_with_parameterized_type(
    struct identmap *im,
    struct ast_generics *g,
    struct ast_typeexpr *type,
    struct ast_typeexpr *partial_type,
    struct ast_typeexpr **materialized_params_out,
    size_t *materialized_params_count_out,
    struct ast_typeexpr *concrete_type_out) {
  enum match_result fail_ret;
  CHECK(g->has_type_params);
  size_t materialized_count = g->params_count;
  struct ast_typeexpr *materialized = malloc_mul(sizeof(*materialized),
                                                 materialized_count);
  for (size_t i = 0; i < materialized_count; i++) {
    materialized[i] = ast_unknown_garbage();
  }

  if (!learn_materializations(im, g, materialized, type, partial_type)) {
    fail_ret = MATCH_NONE;
    goto fail;
  }

  for (size_t i = 0; i < materialized_count; i++) {
    if (!is_complete(&materialized[i])) {
      fail_ret = MATCH_AMBIGUOUSLY;
      goto fail;
    }
  }

  do_replace_generics(g, materialized, materialized_count, type,
                      concrete_type_out);
  *materialized_params_out = materialized;
  *materialized_params_count_out = materialized_count;
  return MATCH_SUCCESS;

 fail:
  SLICE_FREE(materialized, materialized_count, ast_typeexpr_destroy);
  return fail_ret;
}

int typelists_equal(struct identmap *im, struct ast_typeexpr *a, size_t a_count,
                    struct ast_typeexpr *b, size_t b_count) {
  if (a_count != b_count) {
    return 0;
  }

  for (size_t i = 0; i < a_count; i++) {
    if (!exact_typeexprs_equal(im, &a[i], &b[i])) {
      return 0;
    }
  }

  return 1;
}

struct def_instantiation *def_entry_insert_instantiation(
    struct identmap *im,
    struct def_entry *ent,
    struct ast_typeexpr *materialized,
    size_t materialized_count,
    struct ast_typeexpr *concrete_type) {
  CHECK(materialized_count == (ent->generics.has_type_params ?
                               ent->generics.params_count : 0));
  for (size_t i = 0, e = ent->instantiations.count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations.ptr[i];
    if (typelists_equal(im, inst->substitutions, inst->substitutions_count,
                        materialized, materialized_count)) {
      return inst;
    }
  }

  struct ast_typeexpr *copy;
  size_t copy_count;
  SLICE_INIT_COPY(copy, copy_count, materialized, materialized_count,
                  ast_typeexpr_init_copy);

  struct def_instantiation *inst = malloc(sizeof(*inst));
  CHECK(inst);
  def_instantiation_init(inst, ent, &copy, &copy_count, concrete_type);
  def_instantiation_ptr_slice_push(&ent->instantiations, inst);
  return inst;
}

enum match_result def_entry_matches(
    struct identmap *im,
    struct def_entry *ent,
    struct ast_typeexpr *generics_or_null,
    size_t generics_count,
    struct ast_typeexpr *partial_type,
    /* In particular, this affects whether we create
    an instantiation or not, which the caller would
    be responsible for typechecking. */
    int build_return_values,
    struct ast_typeexpr *unified_type_out,
    struct def_instantiation **instantiation_out) {
  if (!ent->generics.has_type_params) {
    if (generics_or_null) {
      return MATCH_NONE;
    }

    if (!unify_directionally(im, partial_type, &ent->type)) {
      return MATCH_NONE;
    }

    if (build_return_values) {
      ast_typeexpr_init_copy(unified_type_out, &ent->type);
      *instantiation_out = def_entry_insert_instantiation(im, ent, NULL, 0,
                                                          &ent->type);
    }
    return MATCH_SUCCESS;
  }

  if (generics_or_null) {
    if (ent->generics.params_count != generics_count) {
      return MATCH_NONE;
    }

    struct ast_typeexpr ent_concrete_type;
    do_replace_generics(&ent->generics, generics_or_null, generics_count,
                        &ent->type, &ent_concrete_type);

    enum match_result ret = MATCH_NONE;
    if (!unify_directionally(im, partial_type, &ent_concrete_type)) {
      goto cleanup_concrete_type;
    }

    ret = MATCH_SUCCESS;
    if (build_return_values) {
      ast_typeexpr_init_copy(unified_type_out, &ent_concrete_type);
      *instantiation_out = def_entry_insert_instantiation(im, ent,
                                                          generics_or_null,
                                                          generics_count,
                                                          &ent_concrete_type);
    }
  cleanup_concrete_type:
    ast_typeexpr_destroy(&ent_concrete_type);
    return ret;
  }

  /* The tricky case:  No generic parameters were given by the expression. */

  CHECK(ent->generics.has_type_params && !generics_or_null);

  struct ast_typeexpr *materialized_params;
  size_t materialized_params_count;
  struct ast_typeexpr concrete_type;
  enum match_result unify_res = unify_with_parameterized_type(
      im,
      &ent->generics,
      &ent->type,
      partial_type,
      &materialized_params,
      &materialized_params_count,
      &concrete_type);
  switch (unify_res) {
  case MATCH_SUCCESS:
    break;
  case MATCH_AMBIGUOUSLY:
  case MATCH_NONE:
    return unify_res;
  default:
    UNREACHABLE();
  }

  if (build_return_values) {
    *unified_type_out = concrete_type;
    *instantiation_out
      = def_entry_insert_instantiation(im, ent,
                                       materialized_params,
                                       materialized_params_count,
                                       unified_type_out);
  } else {
    ast_typeexpr_destroy(&concrete_type);
  }
  SLICE_FREE(materialized_params, materialized_params_count,
             ast_typeexpr_destroy);
  return MATCH_SUCCESS;
}

enum match_result name_table_match_def(
    struct checkstate *cs,
    struct name_table *t,
    struct ast_ident *ident,
    struct ast_typeexpr *generics_or_null,
    size_t generics_count,
    struct ast_typeexpr *partial_type,
    enum report_mode report_mode,
    struct ast_typeexpr *unified_type_out,
    struct def_entry **entry_out,
    struct def_instantiation **instantiation_out) {
  /* matched_type is initialized if matched_ent is non-null. */
  struct ast_typeexpr matched_type;
  struct def_instantiation *matched_instantiation = NULL;
  struct def_entry *matched_ent = NULL;

  ident_value name = ident->value;
  struct defs_by_name_node *node;
  {
    ident_value dbn_id;
    if (identmap_is_interned(&t->defs_by_name, &name, sizeof(name),
                             &dbn_id)) {
      node = identmap_get_user_value(&t->defs_by_name, dbn_id);
      CHECK(node);
    } else {
      node = NULL;
    }
  }

  for (; node; node = node->next) {
    struct def_entry *ent = node->ent;
    CHECK(ent->name == name);

    struct ast_typeexpr unified;
    /* Initialized to NULL to silence a wrong gcc 4.9.2 warning.  (gcc
    is not sure instantiation gets initialized on the code path where
    it gets assigned to matched_instantiation.  (We know it's
    initialized because we pass 1 right here, for the
    build_return_values parameter of def_entry_matches, and because
    the return value is MATCH_SUCCESS.) */
    struct def_instantiation *instantiation = NULL;
    enum match_result res = def_entry_matches(
        cs->im, ent, generics_or_null, generics_count,
        partial_type, 1, &unified, &instantiation);
    switch (res) {
    case MATCH_SUCCESS: {
      if (matched_ent) {
        ast_typeexpr_destroy(&unified);
        if (report_mode & REPORT_MODE_MULTI_MATCH) {
          METERR(cs, ident->meta, "multiple matching '%.*s' definitions\n",
                 IM_P(cs->im, ident->value));
        }
        ast_typeexpr_destroy(&matched_type);
        return MATCH_AMBIGUOUSLY;
      } else {
        matched_type = unified;
        matched_instantiation = instantiation;
        matched_ent = ent;
      }
    } break;
    case MATCH_AMBIGUOUSLY: {
      if (report_mode & REPORT_MODE_MULTI_MATCH) {
        METERR(cs, ident->meta, "ambiguously matching '%.*s' definition\n",
               IM_P(cs->im, ident->value));
      }
      if (matched_ent) {
        ast_typeexpr_destroy(&matched_type);
      }
      return MATCH_AMBIGUOUSLY;
    } break;
    case MATCH_NONE:
      /* Do nothing, move on to next entry. */
      break;
    default:
      UNREACHABLE();
    }
  }

  if (!matched_ent) {
    struct databuf buf;
    databuf_init(&buf);
    sprint_typeexpr(&buf, cs->im, partial_type);
    if (report_mode & REPORT_MODE_NO_MATCH) {
      METERR(cs, ident->meta, "no definition of '%.*s' matches type '%.*s'\n",
             IM_P(cs->im, ident->value), size_to_int(buf.count), buf.buf);
    }
    databuf_destroy(&buf);
    return MATCH_NONE;
  }

  *unified_type_out = matched_type;
  *entry_out = matched_ent;
  *instantiation_out = matched_instantiation;
  return MATCH_SUCCESS;
}

int name_table_lookup_deftype(struct name_table *t,
                              ident_value name,
                              struct generics_arity arity,
                              struct deftype_entry **out) {
  ident_value dtbn_id;
  if (!identmap_is_interned(&t->deftypes_by_name,
                            &name, sizeof(&name),
                            &dtbn_id)) {
    return 0;
  }

  struct deftypes_by_name_node *node
    = identmap_get_user_value(&t->deftypes_by_name, dtbn_id);

  for (; node; node = node->next) {
    struct deftype_entry *ent = node->ent;
    if (ent->name == name && ent->arity.value == arity.value) {
      *out = ent;
      return 1;
    }
  }

  return 0;
}

int name_table_lookup_deftype_inst(struct identmap *im,
                                   struct name_table *t,
                                   ident_value name,
                                   struct ast_typeexpr *generics_or_null,
                                   size_t generics_count,
                                   struct deftype_entry **ent_out,
                                   struct deftype_instantiation **inst_out) {
  struct generics_arity arity = generics_or_null ? param_list_arity(generics_count) : no_param_list_arity();
  struct deftype_entry *ent;
  if (!name_table_lookup_deftype(t, name, arity, &ent)) {
    return 0;
  }

  for (size_t i = 0, e = ent->instantiations.count; i < e; i++) {
    struct deftype_instantiation *inst = ent->instantiations.ptr[i];
    if (typelists_equal(im, inst->substitutions, inst->substitutions_count,
                        generics_or_null, generics_count)) {
      *ent_out = ent;
      *inst_out = inst;
      return 1;
    }
  }

  struct ast_typeexpr *copy;
  size_t copy_count;
  SLICE_INIT_COPY(copy, copy_count, generics_or_null, generics_count, ast_typeexpr_init_copy);

  struct deftype_instantiation *inst = malloc(sizeof(*inst));
  CHECK(inst);
  deftype_instantiation_init(inst, copy, copy_count);
  deftype_instantiation_ptr_slice_push(&ent->instantiations, inst);

  *ent_out = ent;
  *inst_out = inst;
  return 1;
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
