#ifndef KIT_SIZEATTR_H_
#define KIT_SIZEATTR_H_

#include <stdint.h>

#include "identmap.h"
#include "platform.h"

struct ast_typeexpr;
struct name_table;

uint32_t ptr_size(enum target_arch arch);
uint32_t size_size(enum target_arch arch);
uint32_t max_possible_alignof(enum target_arch arch);
uint32_t enum_tag_size(enum target_arch arch);

struct type_attrs {
  uint32_t size;
  uint32_t align;
  int is_primitive;
};

uint32_t gp_sizeof(struct name_table *nt, struct ast_typeexpr *type);
uint32_t gp_alignof(struct name_table *nt, struct ast_typeexpr *type);
struct type_attrs gp_attrsof(struct name_table *nt, struct ast_typeexpr *type);

void gp_field_sizeoffset(struct name_table *nt, struct ast_typeexpr *type,
                         ident_value field_name, uint32_t *sizeof_out,
                         uint32_t *offsetof_out);

#endif /* KIT_SIZEATTR_H_ */

