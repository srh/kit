#ifndef KIRA_TYPEEXPR_H_
#define KIRA_TYPEEXPR_H_

struct ast_typeexpr;
struct databuf;
struct identmap;

void sprint_typeexpr(struct databuf *buf, struct identmap *im, struct ast_typeexpr *a);

void DBG_typeexpr(const char *msg, struct identmap *im, struct ast_typeexpr *a);

#endif /* KIRA_TYPEEXPR_H_ */

