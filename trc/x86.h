#ifndef KIT_X86_H_
#define KIT_X86_H_

#include <stdint.h>

#include "identmap.h"
#include "platform.h"

struct ast_typeexpr;
struct name_table;

/* TODO(): This is an honorary use of X86.  Fix all uses. */
#define DWORD_SIZE 4

uint32_t ptr_size(enum target_platform platform);

struct type_attrs {
  uint32_t size;
  uint32_t align;
  int is_primitive;
};

uint32_t x86_sizeof(struct name_table *nt, struct ast_typeexpr *type);
uint32_t x86_alignof(struct name_table *nt, struct ast_typeexpr *type);
struct type_attrs x86_attrsof(struct name_table *nt, struct ast_typeexpr *type);

void x86_field_sizeoffset(struct name_table *nt, struct ast_typeexpr *type,
                          ident_value field_name, uint32_t *sizeof_out,
                          uint32_t *offsetof_out);

#endif /* KIT_X86_H_ */

