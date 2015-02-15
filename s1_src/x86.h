#ifndef KIRA_X86_H_
#define KIRA_X86_H_

#include <stdint.h>

#include "identmap.h"

struct ast_typeexpr;
struct name_table;

#define DWORD_SIZE 4

void kira_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      uint32_t *sizeof_out, uint32_t *alignof_out);

uint32_t kira_sizeof(struct name_table *nt, struct ast_typeexpr *type);
uint32_t kira_alignof(struct name_table *nt, struct ast_typeexpr *type);

void kira_field_sizeoffset(struct name_table *nt, struct ast_typeexpr *type,
                           ident_value field_name, uint32_t *sizeof_out,
                           uint32_t *offsetof_out);

#endif /* KIRA_X86_H_ */

