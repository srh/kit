#ifndef KIRA_OPGRAPH_H_
#define KIRA_OPGRAPH_H_

#include <stddef.h>

struct ast_typeexpr;
struct opnode;
struct varnode;

struct opgraph {
  struct opnode *ops;
  size_t ops_count;
  size_t ops_limit;

  struct varnode *vars;
  size_t vars_count;
  size_t vars_limit;
};

struct opnum { size_t value; };
struct opnum opnum_invalid(void);
int opnum_is_valid(struct opnum);

struct varnum { size_t value; };

struct varnum varnum_invalid(void);
int varnum_is_valid(struct varnum);

struct varnum opgraph_add_var(struct opgraph *g, struct ast_typeexpr *type);

struct funcgraph {
  struct opgraph opg;

  struct varnum entry_point;
  struct varnum *arg_vars;
  size_t arg_vars_count;
  struct varnum return_var;
};

void funcgraph_init(struct funcgraph *g);
void funcgraph_init_move(struct funcgraph *g, struct funcgraph *movee);
void funcgraph_destroy(struct funcgraph *g);


#endif /* KIRA_OPGRAPH_H_ */
