#ifndef KIT_TABLE_H_
#define KIT_TABLE_H_

#include "ast.h"
#include "arity.h"
#include "primitive.h"
#include "objfile_objfile.h"
#include "static_value.h"

struct checkstate;

struct def_instantiation {
  /* Sigh, backpointers. */
  struct def_entry *owner;

  int typecheck_started;

  /* The types used to substitute the respective def_entry generics
  params. */
  struct ast_typeexpr *substitutions;
  size_t substitutions_count;

  /* The concrete type of this instantiation. */
  struct ast_typeexpr type;

  int annotated_rhs_computed;
  struct ast_expr annotated_rhs;

  int value_computed;
  struct static_value value;

  int symbol_table_index_computed;
  struct sti symbol_table_index;
};

struct ast_expr *di_annotated_rhs(struct def_instantiation *inst);
void di_set_annotated_rhs(struct def_instantiation *inst,
                          struct ast_expr annotated_rhs);

struct static_value *di_value(struct def_instantiation *inst);
struct static_value *di_value_for_set(struct def_instantiation *inst);

struct sti di_symbol_table_index(struct def_instantiation *inst);
void di_set_symbol_table_index(struct def_instantiation *inst,
                               struct sti symbol_table_index);

struct defclass_ident {
  ident_value name;
  struct generics_arity arity;
};

struct def_entry {
  ident_value name;
  struct ast_generics generics;
  struct ast_typeexpr type;

  /* The access scopes this entry has _access_ to. */
  struct defclass_ident *accessible;
  size_t accessible_count;

  /* The access scopes you need to access this entry. */
  struct defclass_ident *private_to;
  size_t private_to_count;

  int is_primitive;
  struct primitive_op primitive_op;
  int is_extern;
  int is_export;
  struct ast_def *def;

  struct def_instantiation **instantiations;
  size_t instantiations_count;
  size_t instantiations_limit;

  /* Names of things whose value each def references at compile-time
  evaluation.  We could do this per-instantiation but right now that's
  irrelevant because there's no specialization.  These references must
  form an acyclic graph -- or the user's program is invalid. */
  struct def_entry **static_references;
  size_t static_references_count;
  size_t static_references_limit;

  int known_acyclic;
  int acyclicity_being_chased;
};

void def_entry_note_static_reference(struct def_entry *ent,
                                     struct def_entry *referent);


enum typeexpr_trait {
  TYPEEXPR_TRAIT_LACKED,
  TYPEEXPR_TRAIT_HAD,
  TYPEEXPR_TRAIT_TRIVIALLY_HAD,
};

struct typeexpr_traits {
  enum typeexpr_trait movable;
  enum typeexpr_trait copyable;
  enum typeexpr_trait inittible;
};

struct typeexpr_trait_instantiations {
  struct def_instantiation *move_inst;
  struct def_instantiation *copy_inst;
  struct def_instantiation *destroy_inst;
  struct def_instantiation *init_inst;
};

struct deftype_instantiation {
  /* The types used to substitute the respective deftype_entry generics
  params. */
  struct ast_typeexpr *substitutions;
  size_t substitutions_count;

  int has_typeexpr_traits;
  struct typeexpr_traits typeexpr_traits;
  struct typeexpr_trait_instantiations explicit_trait_instantiations;
  struct ast_deftype_rhs concrete_rhs;
};

struct deftype_entry {
  ident_value name;
  struct generics_arity arity;

  /* Non-null, if !arity_no_paramlist(arity) and positive arity. */
  int *flatly_held;
  /* Equal to arity.value, if it has a value. */
  size_t flatly_held_count;

  struct deftype_instantiation **instantiations;
  size_t instantiations_count;
  size_t instantiations_limit;

  int has_been_checked;
  int is_being_checked;

  int is_primitive;
  uint32_t primitive_sizeof;
  uint32_t primitive_alignof;
  struct ast_deftype *deftype;
};

int deftype_entry_param_is_flatly_held(struct deftype_entry *entry,
                                       size_t which_generic);

struct name_table {
  struct arena arena;

  struct def_entry **defs;
  size_t defs_count;
  size_t defs_limit;

  struct identmap defs_by_name;

  struct deftype_entry **deftypes;
  size_t deftypes_count;
  size_t deftypes_limit;

  struct identmap deftypes_by_name;
};

void name_table_init(struct name_table *t);
void name_table_destroy(struct name_table *t);

int name_table_add_def(struct identmap *im,
                       struct name_table *t,
                       ident_value name,
                       struct ast_generics *generics,
                       struct ast_typeexpr *type,
                       struct defclass_ident *accessible,
                       size_t accessible_count,
                       int is_export,
                       struct ast_def *def);
int name_table_add_primitive_def(
    struct identmap *im,
    struct name_table *t,
    ident_value name,
    struct primitive_op primitive_op,
    struct ast_generics *generics,
    struct ast_typeexpr *type);
/* private_to_count is how many access scopes you need to get access
to the type.  So if it's zero, this is a public def. */
int name_table_add_private_primitive_def(struct identmap *im,
                                         struct name_table *t,
                                         ident_value name,
                                         struct primitive_op primitive_op,
                                         struct ast_generics *generics,
                                         struct ast_typeexpr *type,
                                         struct defclass_ident *private_to,
                                         size_t private_to_count);

int name_table_add_extern_def(struct identmap *im,
                              struct name_table *t,
                              ident_value name,
                              struct ast_typeexpr *type);
int name_table_add_deftype(struct identmap *im,
                           struct name_table *t,
                           ident_value name,
                           struct generics_arity arity,
                           struct ast_deftype *deftype);
int name_table_add_primitive_type(struct identmap *im,
                                  struct name_table *t,
                                  ident_value name,
                                  int *flatly_held,
                                  size_t flatly_held_count,
                                  uint32_t primitive_sizeof,
                                  uint32_t primitive_alignof);

enum report_mode {
  REPORT_MODE_MULTI_MATCH = 1,
  REPORT_MODE_NO_MATCH = 2,
  REPORT_MODE_ALL = REPORT_MODE_MULTI_MATCH | REPORT_MODE_NO_MATCH,
  REPORT_MODE_NONE = 0,
};

enum match_result {
  MATCH_SUCCESS,
  MATCH_AMBIGUOUSLY,
  MATCH_NONE,
};

enum match_result name_table_match_def(
    struct checkstate *cs,
    struct name_table *t,
    struct ast_ident *name,
    struct ast_typeexpr *generics_or_null,
    size_t generics_count,
    struct ast_typeexpr *partial_type,
    enum report_mode report_mode,
    struct ast_typeexpr *unified_type_out,
    struct def_entry **entry_out,
    struct def_instantiation **instantiation_out);

int name_table_lookup_deftype(struct name_table *t,
                              ident_value name,
                              struct generics_arity arity,
                              struct deftype_entry **out);
int name_table_lookup_deftype_inst(struct identmap *im,
                                   struct name_table *t,
                                   ident_value name,
                                   struct ast_typeexpr *generics_or_null,
                                   size_t generics_count,
                                   struct deftype_entry **ent_out,
                                   struct deftype_instantiation **inst_out);

int name_table_shadowed(struct name_table *t, ident_value name);

struct deftype_entry *lookup_deftype(struct name_table *t,
                                     struct ast_deftype *a);

void deftype_entry_mark_is_being_checked(struct deftype_entry *ent);
void deftype_entry_mark_has_been_checked(struct deftype_entry *ent);
void deftype_entry_mark_generic_flatly_held(struct deftype_entry *ent,
                                            size_t which_generic);

int typelists_equal(struct identmap *im, struct ast_typeexpr *a, size_t a_count,
                    struct ast_typeexpr *b, size_t b_count);

int is_complete(struct ast_typeexpr *type);
int is_numeric_type(struct identmap *im, struct ast_typeexpr *a);
int combine_partial_types(struct identmap *im,
                          struct ast_typeexpr *a,
                          struct ast_typeexpr *b,
                          struct ast_typeexpr *out);


#endif /* KIT_TABLE_H_ */
