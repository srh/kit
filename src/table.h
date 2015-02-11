#ifndef KIRA_TABLE_H_
#define KIRA_TABLE_H_

#include "ast.h"
#include "arity.h"

enum primitive_op {
  PRIMITIVE_OP_CONVERT_U8_TO_U8,
  PRIMITIVE_OP_CONVERT_U8_TO_I8,
  PRIMITIVE_OP_CONVERT_U8_TO_U16,
  PRIMITIVE_OP_CONVERT_U8_TO_I16,
  PRIMITIVE_OP_CONVERT_U8_TO_U32,
  PRIMITIVE_OP_CONVERT_U8_TO_I32,

  PRIMITIVE_OP_CONVERT_I8_TO_U8,
  PRIMITIVE_OP_CONVERT_I8_TO_I8,
  PRIMITIVE_OP_CONVERT_I8_TO_U16,
  PRIMITIVE_OP_CONVERT_I8_TO_I16,
  PRIMITIVE_OP_CONVERT_I8_TO_U32,
  PRIMITIVE_OP_CONVERT_I8_TO_I32,

  PRIMITIVE_OP_CONVERT_U16_TO_U8,
  PRIMITIVE_OP_CONVERT_U16_TO_I8,
  PRIMITIVE_OP_CONVERT_U16_TO_U16,
  PRIMITIVE_OP_CONVERT_U16_TO_I16,
  PRIMITIVE_OP_CONVERT_U16_TO_U32,
  PRIMITIVE_OP_CONVERT_U16_TO_I32,

  PRIMITIVE_OP_CONVERT_I16_TO_U8,
  PRIMITIVE_OP_CONVERT_I16_TO_I8,
  PRIMITIVE_OP_CONVERT_I16_TO_U16,
  PRIMITIVE_OP_CONVERT_I16_TO_I16,
  PRIMITIVE_OP_CONVERT_I16_TO_U32,
  PRIMITIVE_OP_CONVERT_I16_TO_I32,

  PRIMITIVE_OP_CONVERT_U32_TO_U8,
  PRIMITIVE_OP_CONVERT_U32_TO_I8,
  PRIMITIVE_OP_CONVERT_U32_TO_U16,
  PRIMITIVE_OP_CONVERT_U32_TO_I16,
  PRIMITIVE_OP_CONVERT_U32_TO_U32,
  PRIMITIVE_OP_CONVERT_U32_TO_I32,

  PRIMITIVE_OP_CONVERT_I32_TO_U8,
  PRIMITIVE_OP_CONVERT_I32_TO_I8,
  PRIMITIVE_OP_CONVERT_I32_TO_U16,
  PRIMITIVE_OP_CONVERT_I32_TO_I16,
  PRIMITIVE_OP_CONVERT_I32_TO_U32,
  PRIMITIVE_OP_CONVERT_I32_TO_I32,

  PRIMITIVE_OP_NEGATE_I8,
  PRIMITIVE_OP_NEGATE_I16,
  PRIMITIVE_OP_NEGATE_I32,

  PRIMITIVE_OP_LOGICAL_NOT,

  /* TODO: Idk about this. */
  PRIMITIVE_OP_SIZEOF,
  PRIMITIVE_OP_ALIGNOF,

  PRIMITIVE_OP_BIT_NOT_I8,
  PRIMITIVE_OP_BIT_NOT_U8,
  PRIMITIVE_OP_BIT_NOT_I16,
  PRIMITIVE_OP_BIT_NOT_U16,
  PRIMITIVE_OP_BIT_NOT_I32,
  PRIMITIVE_OP_BIT_NOT_U32,

  PRIMITIVE_OP_ADD_U8,
  PRIMITIVE_OP_SUB_U8,
  PRIMITIVE_OP_MUL_U8,
  PRIMITIVE_OP_DIV_U8,
  PRIMITIVE_OP_MOD_U8,
  PRIMITIVE_OP_LT_U8,
  PRIMITIVE_OP_LE_U8,
  PRIMITIVE_OP_GT_U8,
  PRIMITIVE_OP_GE_U8,
  PRIMITIVE_OP_EQ_U8,
  PRIMITIVE_OP_NE_U8,
  PRIMITIVE_OP_BIT_XOR_U8,
  PRIMITIVE_OP_BIT_OR_U8,
  PRIMITIVE_OP_BIT_AND_U8,
  PRIMITIVE_OP_BIT_LEFTSHIFT_U8,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_U8,

  PRIMITIVE_OP_ADD_I8,
  PRIMITIVE_OP_SUB_I8,
  PRIMITIVE_OP_MUL_I8,
  PRIMITIVE_OP_DIV_I8,
  PRIMITIVE_OP_MOD_I8,
  PRIMITIVE_OP_LT_I8,
  PRIMITIVE_OP_LE_I8,
  PRIMITIVE_OP_GT_I8,
  PRIMITIVE_OP_GE_I8,
  PRIMITIVE_OP_EQ_I8,
  PRIMITIVE_OP_NE_I8,
  PRIMITIVE_OP_BIT_XOR_I8,
  PRIMITIVE_OP_BIT_OR_I8,
  PRIMITIVE_OP_BIT_AND_I8,
  PRIMITIVE_OP_BIT_LEFTSHIFT_I8,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_I8,

  PRIMITIVE_OP_ADD_U16,
  PRIMITIVE_OP_SUB_U16,
  PRIMITIVE_OP_MUL_U16,
  PRIMITIVE_OP_DIV_U16,
  PRIMITIVE_OP_MOD_U16,
  PRIMITIVE_OP_LT_U16,
  PRIMITIVE_OP_LE_U16,
  PRIMITIVE_OP_GT_U16,
  PRIMITIVE_OP_GE_U16,
  PRIMITIVE_OP_EQ_U16,
  PRIMITIVE_OP_NE_U16,
  PRIMITIVE_OP_BIT_XOR_U16,
  PRIMITIVE_OP_BIT_OR_U16,
  PRIMITIVE_OP_BIT_AND_U16,
  PRIMITIVE_OP_BIT_LEFTSHIFT_U16,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_U16,

  PRIMITIVE_OP_ADD_I16,
  PRIMITIVE_OP_SUB_I16,
  PRIMITIVE_OP_MUL_I16,
  PRIMITIVE_OP_DIV_I16,
  PRIMITIVE_OP_MOD_I16,
  PRIMITIVE_OP_LT_I16,
  PRIMITIVE_OP_LE_I16,
  PRIMITIVE_OP_GT_I16,
  PRIMITIVE_OP_GE_I16,
  PRIMITIVE_OP_EQ_I16,
  PRIMITIVE_OP_NE_I16,
  PRIMITIVE_OP_BIT_XOR_I16,
  PRIMITIVE_OP_BIT_OR_I16,
  PRIMITIVE_OP_BIT_AND_I16,
  PRIMITIVE_OP_BIT_LEFTSHIFT_I16,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_I16,

  PRIMITIVE_OP_ADD_U32,
  PRIMITIVE_OP_SUB_U32,
  PRIMITIVE_OP_MUL_U32,
  PRIMITIVE_OP_DIV_U32,
  PRIMITIVE_OP_MOD_U32,
  PRIMITIVE_OP_LT_U32,
  PRIMITIVE_OP_LE_U32,
  PRIMITIVE_OP_GT_U32,
  PRIMITIVE_OP_GE_U32,
  PRIMITIVE_OP_EQ_U32,
  PRIMITIVE_OP_NE_U32,
  PRIMITIVE_OP_BIT_XOR_U32,
  PRIMITIVE_OP_BIT_OR_U32,
  PRIMITIVE_OP_BIT_AND_U32,
  PRIMITIVE_OP_BIT_LEFTSHIFT_U32,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_U32,

  PRIMITIVE_OP_ADD_I32,
  PRIMITIVE_OP_SUB_I32,
  PRIMITIVE_OP_MUL_I32,
  PRIMITIVE_OP_DIV_I32,
  PRIMITIVE_OP_MOD_I32,
  PRIMITIVE_OP_LT_I32,
  PRIMITIVE_OP_LE_I32,
  PRIMITIVE_OP_GT_I32,
  PRIMITIVE_OP_GE_I32,
  PRIMITIVE_OP_EQ_I32,
  PRIMITIVE_OP_NE_I32,
  PRIMITIVE_OP_BIT_XOR_I32,
  PRIMITIVE_OP_BIT_OR_I32,
  PRIMITIVE_OP_BIT_AND_I32,
  PRIMITIVE_OP_BIT_LEFTSHIFT_I32,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_I32,
};

#define PRIMITIVE_OP_INVALID ((enum primitive_op)-1)

enum static_value_tag {
  STATIC_VALUE_I32,
  STATIC_VALUE_U32,
  STATIC_VALUE_U8,
  STATIC_VALUE_LAMBDA,
  STATIC_VALUE_PRIMITIVE_OP,
};

struct static_value {
  enum static_value_tag tag;
  union {
    int32_t i32_value;
    uint32_t u32_value;
    uint8_t u8_value;
    int8_t i8_value;
    /* An owned ref to the _typechecked_, annotated AST. */
    struct ast_expr typechecked_lambda;
    enum primitive_op primitive_op;
  } u;
};

void static_value_init_i32(struct static_value *a, int32_t i32_value);
void static_value_init_u32(struct static_value *a, uint32_t u32_value);
void static_value_init_u8(struct static_value *a, uint8_t u8_value);
void static_value_init_typechecked_lambda(struct static_value *a,
                                          struct ast_expr lambda);
void static_value_init_primitive_op(struct static_value *a,
                                    enum primitive_op primitive_op);
void static_value_init_copy(struct static_value *a, struct static_value *c);
void static_value_destroy(struct static_value *a);

struct def_instantiation {
  /* TODO: Sigh, backpointers. */
  struct def_entry *owner;

  int typecheck_started;
  struct ast_typeexpr *substitutions;
  size_t substitutions_count;
  struct ast_typeexpr type;

  int annotated_rhs_computed;
  struct ast_expr annotated_rhs;

  int value_computed;
  struct static_value value;

  int symbol_table_index_computed;
  uint32_t symbol_table_index;
};

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
  enum primitive_op primitive_op;
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

struct deftype_entry {
  ident_value name;
  struct generics_arity arity;

  /* Non-null, if !arity_no_paramlist(arity) and positive arity. */
  int *flatly_held;
  /* Equal to arity.value, if it has a value. */
  size_t flatly_held_count;

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

int name_table_add_def(struct name_table *t,
                       ident_value name,
                       struct ast_generics *generics,
                       struct ast_typeexpr *type,
                       struct defclass_ident *accessible,
                       size_t accessible_count,
                       int is_export,
                       struct ast_def *def);
int name_table_add_primitive_def(
    struct name_table *t,
    ident_value name,
    enum primitive_op primitive_op,
    struct ast_generics *generics,
    struct ast_typeexpr *type);
/* private_to_count is how many access scopes you need to get access
to the type.  So if it's zero, this is a public def. */
int name_table_add_private_primitive_def(struct name_table *t,
                                         ident_value name,
                                         enum primitive_op primitive_op,
                                         struct ast_generics *generics,
                                         struct ast_typeexpr *type,
                                         struct defclass_ident *private_to,
                                         size_t private_to_count);

int name_table_add_extern_def(struct name_table *t,
                              ident_value name,
                              struct ast_typeexpr *type);
int name_table_add_deftype(struct name_table *t,
                           ident_value name,
                           struct generics_arity arity,
                           struct ast_deftype *deftype);
int name_table_add_primitive_type(struct name_table *t,
                                  ident_value name,
                                  int *flatly_held,
                                  size_t flatly_held_count,
                                  uint32_t primitive_sizeof,
                                  uint32_t primitive_alignof);

int name_table_match_def(struct name_table *t,
                         struct ast_ident *name,
                         struct ast_typeexpr *generics_or_null,
                         size_t generics_count,
                         struct ast_typeexpr *partial_type,
                         int *zero_defs_out,
                         struct ast_typeexpr *unified_type_out,
                         struct def_entry **entry_out,
                         struct def_instantiation **instantiation_out);

size_t name_table_count_matching_defs(struct name_table *t,
                                      struct ast_ident *name,
                                      struct ast_typeexpr *generics_or_null,
                                      size_t generics_count,
                                      struct ast_typeexpr *partial_type);

int name_table_lookup_deftype(struct name_table *t,
                              ident_value name,
                              struct generics_arity arity,
                              struct deftype_entry **out);

int name_table_shadowed(struct name_table *t, ident_value name);

struct deftype_entry *lookup_deftype(struct name_table *t,
                                     struct ast_deftype *a);

void deftype_entry_mark_is_being_checked(struct deftype_entry *ent);
void deftype_entry_mark_has_been_checked(struct deftype_entry *ent);
void deftype_entry_mark_generic_flatly_held(struct deftype_entry *ent,
                                            size_t which_generic);

int typelists_equal(struct ast_typeexpr *a, size_t a_count,
                    struct ast_typeexpr *b, size_t b_count);

#endif /* KIRA_TABLE_H_ */
