#ifndef KIRA_TYPECHECK_H_
#define KIRA_TYPECHECK_H_

#include <stddef.h>
#include <stdint.h>

#include "checkstate.h"
#include "identmap.h"

#define METERR(loc, fmt, ...) ERR("At %"PRIz":%"PRIz": " fmt, (loc).pos_start.line, (loc).pos_start.column, __VA_ARGS__)
/* TODO: QMETERR statements are... questionable. */
#define QMETERR(loc, fmt, ...) ERR("At %"PRIz":%"PRIz": " fmt, (loc).pos_start.line, (loc).pos_start.column, __VA_ARGS__)

#define VOID_TYPE_NAME "void"
#define U8_TYPE_NAME "u8"
#define I8_TYPE_NAME "i8"
#define U16_TYPE_NAME "u16"
#define I16_TYPE_NAME "i16"
#define I32_TYPE_NAME "i32"
#define U32_TYPE_NAME "u32"
#define PTR_TYPE_NAME "ptr"
#define FUNC_TYPE_NAME "func"

/* TODO: Later add a distinct bool type. */
#define BOOLEAN_STANDIN_TYPE_NAME I8_TYPE_NAME
#define KIRA_BOOL_SIZE 1

#define CHAR_STANDIN_TYPE_NAME U8_TYPE_NAME

#define SIZE_STANDIN_TYPE_NAME U32_TYPE_NAME

enum numeric_type {
  NUMERIC_TYPE_U8,
  NUMERIC_TYPE_I8,
  NUMERIC_TYPE_U16,
  NUMERIC_TYPE_I16,
  NUMERIC_TYPE_I32,
  NUMERIC_TYPE_U32,
};

uint32_t numeric_type_size(enum numeric_type t);

struct ast_generics;
struct ast_typeexpr;

typedef int module_loader(const uint8_t *module_name,
                          size_t module_name_count,
                          uint8_t **data_out,
                          size_t *data_count_out);

int chase_modules_and_typecheck(struct checkstate *cs,
                                module_loader *loader,
                                ident_value first_module);

int test_check_file(void);

int read_module_file(const uint8_t *module_name,
                     size_t module_name_count,
                     uint8_t **data_out,
                     size_t *data_size_out);

int unify_directionally(struct ast_typeexpr *partial_type,
                        struct ast_typeexpr *complete_type);
int generics_lookup_name(struct ast_generics *a,
                         ident_value name,
                         size_t *index_out);
struct ast_ident make_ast_ident(ident_value ident);
void init_name_type(struct ast_typeexpr *a, ident_value name);
int exact_typeexprs_equal(struct ast_typeexpr *a, struct ast_typeexpr *b);
int typeexpr_is_func_type(struct identmap *im, struct ast_typeexpr *x);
int view_ptr_target(struct identmap *im,
                    struct ast_typeexpr *ptr_type,
                    struct ast_typeexpr **target_out);
void wrap_in_ptr(struct identmap *im,
                 struct ast_typeexpr *target,
                 struct ast_typeexpr *ptr_out);
void do_replace_generics(struct ast_generics *generics,
                         struct ast_typeexpr *generics_substitutions,
                         size_t generics_substitutions_count,
                         struct ast_typeexpr *a,
                         struct ast_typeexpr *out);
void do_replace_rhs_generics(struct ast_generics *generics,
                             struct ast_typeexpr *generics_substitutions,
                             size_t generics_substitutions_count,
                             struct ast_rhs *a,
                             struct ast_rhs *out);
struct ast_typeexpr *expose_func_return_type(struct identmap *im,
                                             struct ast_typeexpr *func,
                                             size_t expected_params_count);
void numeric_literal_type(struct identmap *im,
                          struct ast_numeric_literal *a,
                          struct ast_typeexpr *out);
int numeric_literal_to_u32(int8_t *digits, size_t digits_count,
                           uint32_t *out);
int numeric_literal_to_i32(int8_t *digits, size_t digits_count,
                           int32_t *out);

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

struct exprscope;
int check_typeexpr_traits(struct checkstate *cs,
                          /* a is a concrete type. */
                          struct ast_typeexpr *a,
                          struct exprscope *also_typecheck,
                          struct typeexpr_traits *out);
int check_typeexpr_name_traits(struct checkstate *cs,
                               struct ast_typeexpr *a,
                               struct exprscope *also_typecheck,
                               struct typeexpr_traits *out,
                               struct typeexpr_trait_instantiations *insts_out,
                               int *has_concrete_deftype_rhs_out_or_null,
                               struct ast_rhs *concrete_deftype_rhs_out_or_null);
int check_typeexpr_app_traits(struct checkstate *cs,
                              struct ast_typeexpr *a,
                              struct exprscope *also_typecheck,
                              struct typeexpr_traits *out,
                              struct typeexpr_trait_instantiations *insts_out,
                              int *has_concrete_deftype_rhs_out_or_null,
                              struct ast_rhs *concrete_deftype_rhs_out_or_null);

#endif /* KIRA_TYPECHECK_H_ */
