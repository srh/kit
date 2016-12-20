#ifndef KIT_PRINT_H_
#define KIT_PRINT_H_

#include <stddef.h>

struct ast_typeexpr;
struct databuf;
struct identmap;

void sprint_typeexpr(struct databuf *b, struct identmap *im, struct ast_typeexpr *a);
void sprint_type_param_list(struct databuf *b, struct identmap *im, struct ast_typeexpr *types, size_t types_count);

void DBG_typeexpr(const char *msg, struct identmap *im, struct ast_typeexpr *a);

#endif /* KIT_PRINT_H_ */

