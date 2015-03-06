#ifndef KIRA_PRINT_H_
#define KIRA_PRINT_H_

struct ast_typeexpr;
struct databuf;
struct identmap;

void sprint_typeexpr(struct databuf *buf, struct identmap *im, struct ast_typeexpr *a);

void DBG_typeexpr(const char *msg, struct identmap *im, struct ast_typeexpr *a);

#endif /* KIRA_PRINT_H_ */

