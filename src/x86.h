#ifndef KIRA_X86_H_
#define KIRA_X86_H_

#include <stdint.h>

struct ast_typeexpr;
struct name_table;

#define DWORD_SIZE 4

void kira_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      uint32_t *sizeof_out, uint32_t *alignof_out);

uint32_t kira_sizeof(struct name_table *nt, struct ast_typeexpr *type);

#endif /* KIRA_X86_H_ */

