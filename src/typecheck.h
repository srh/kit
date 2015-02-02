#ifndef KIRA_TYPECHECK_H_
#define KIRA_TYPECHECK_H_

#include <stddef.h>
#include <stdint.h>

#include "checkstate.h"
#include "identmap.h"

#define VOID_TYPE_NAME "void"
#define U8_TYPE_NAME "u8"
#define I32_TYPE_NAME "i32"
#define U32_TYPE_NAME "u32"
#define PTR_TYPE_NAME "ptr"
#define FUNC_TYPE_NAME "func"
#define BOOLEAN_STANDIN_TYPE_NAME I32_TYPE_NAME
#define KIRA_BOOL_SIZE 4

enum numeric_type {
  NUMERIC_TYPE_U8,
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

/* TODO: Move these functions elsewhere. */
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
void do_replace_generics(struct ast_generics *generics,
                         struct ast_typeexpr *generics_substitutions,
                         struct ast_typeexpr *a,
                         struct ast_typeexpr *out);
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
#endif /* KIRA_TYPECHECK_H_ */
