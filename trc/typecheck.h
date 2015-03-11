#ifndef KIRA_TYPECHECK_H_
#define KIRA_TYPECHECK_H_

#include <stddef.h>
#include <stdint.h>

#include "checkstate.h"
#include "identmap.h"

struct ast_generics;
struct ast_typeexpr;
struct ast_numeric_literal;

#define METERR(cs, loc, fmt, ...) ERR("At %.*s:%"PRIz":%"PRIz": " fmt, IM_P((cs)->im, checkstate_g_o_import_name((cs), (loc).pos_start.global_offset)), checkstate_g_o_line((cs), (loc).pos_start.global_offset), checkstate_g_o_column((cs), (loc).pos_start.global_offset), __VA_ARGS__)

#define VOID_TYPE_NAME "void"
#define U8_TYPE_NAME "u8"
#define I8_TYPE_NAME "i8"
#define U16_TYPE_NAME "u16"
#define I16_TYPE_NAME "i16"
#define U32_TYPE_NAME "u32"
#define I32_TYPE_NAME "i32"
#define SIZE_TYPE_NAME "size"
/* osize arithmetic doesn't check overflow. */
#define OSIZE_TYPE_NAME "osize"
#define PTR_TYPE_NAME "ptr"
#define FUNC_TYPE_NAME "fn"
#define BOOL_TYPE_NAME "bool"

#define ARRAY_LENGTH_FIELDNAME "length"

#define KIRA_BOOL_SIZE 1

#define CHAR_STANDIN_TYPE_NAME U8_TYPE_NAME

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

int unify_directionally(struct identmap *im,
                        struct ast_typeexpr *partial_type,
                        struct ast_typeexpr *complete_type);
int generics_lookup_name(struct ast_generics *a,
                         ident_value name,
                         size_t *index_out);
struct ast_ident make_ast_ident(ident_value ident);
void init_name_type(struct ast_typeexpr *a, ident_value name);
int exact_typeexprs_equal(struct identmap *im, struct ast_typeexpr *a,
                          struct ast_typeexpr *b);
int typeexpr_is_func_type(struct identmap *im, struct ast_typeexpr *x);
int view_ptr_target(struct common_idents *cm,
                    struct ast_typeexpr *ptr_type,
                    struct ast_typeexpr **target_out);
void do_replace_generics(struct ast_generics *generics,
                         struct ast_typeexpr *generics_substitutions,
                         size_t generics_substitutions_count,
                         struct ast_typeexpr *a,
                         struct ast_typeexpr *out);
void do_replace_rhs_generics(struct ast_generics *generics,
                             struct ast_typeexpr *generics_substitutions,
                             size_t generics_substitutions_count,
                             struct ast_deftype_rhs *a,
                             struct ast_deftype_rhs *out);
struct ast_typeexpr *expose_func_return_type(struct common_idents *cm,
                                             struct ast_typeexpr *func,
                                             size_t expected_params_count);
uint32_t unsafe_numeric_literal_u32(struct ast_numeric_literal *a);
int numeric_literal_to_u32(struct ast_numeric_literal *a, uint32_t *out);
int squash_u32_to_i32(uint32_t value, int32_t *out);
int squash_u32_to_u8(uint32_t value, uint8_t *out);
int squash_u32_to_i8(uint32_t value, int8_t *out);

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
                               struct ast_deftype_rhs *concrete_deftype_rhs_out_or_null);
int check_typeexpr_app_traits(struct checkstate *cs,
                              struct ast_typeexpr *a,
                              struct exprscope *also_typecheck,
                              struct typeexpr_traits *out,
                              struct typeexpr_trait_instantiations *insts_out,
                              int *has_concrete_deftype_rhs_out_or_null,
                              struct ast_deftype_rhs *concrete_deftype_rhs_out_or_null);

#endif /* KIRA_TYPECHECK_H_ */
