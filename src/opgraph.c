#include "opgraph.h"

#include "ast.h"
#include "slice.h"

#define VARNUM_INVALID_VALUE SIZE_MAX
#define OPNUM_INVALID_VALUE SIZE_MAX

struct opnum opnum_invalid(void) {
  struct opnum ret;
  ret.value = OPNUM_INVALID_VALUE;
  return ret;
}

int opnum_is_valid(struct opnum v) {
  return v.value != OPNUM_INVALID_VALUE;
}


struct varnum varnum_invalid(void) {
  struct varnum ret;
  ret.value = VARNUM_INVALID_VALUE;
  return ret;
}

int varnum_is_valid(struct varnum v) {
  return v.value != VARNUM_INVALID_VALUE;
}


enum opnode_tag {
  /* NOP nodes just redirect you to another node -- often we fill in
     the NOP node's "next node" later, when constructing the opgraph. */
  OPNODE_NOP,
  /* Returns from the function -- there is no "next node". */
  OPNODE_RETURN,
};

struct opnode {
  enum opnode_tag tag;
  union {
    size_t nop_next;
  } u;
};

void opnode_destroy(struct opnode *n) {
  switch (n->tag) {
  case OPNODE_NOP: break;
  case OPNODE_RETURN: break;
  default:
    UNREACHABLE();
  }
  n->tag = (enum opnode_tag)-1;
}

struct varnode {
  struct ast_typeexpr type;
};

void varnode_init(struct varnode *v, struct ast_typeexpr *type) {
  ast_typeexpr_init_copy(&v->type, type);
}

void varnode_destroy(struct varnode *v) {
  ast_typeexpr_destroy(&v->type);
}

void opgraph_init(struct opgraph *g) {
  g->ops = NULL;
  g->ops_count = 0;
  g->ops_limit = 0;

  g->vars = NULL;
  g->vars_count = 0;
  g->vars_limit = 0;
}

void opgraph_init_move(struct opgraph *g, struct opgraph *movee) {
  *g = *movee;
  opgraph_init(movee);
}

void opgraph_destroy(struct opgraph *g) {
  SLICE_FREE(g->ops, g->ops_count, opnode_destroy);
  g->ops_limit = 0;
  SLICE_FREE(g->vars, g->vars_count, varnode_destroy);
  g->vars_limit = 0;
}

struct varnum opgraph_add_var(struct opgraph *g, struct ast_typeexpr *type) {
  struct varnode node;
  varnode_init(&node, type);
  struct varnum vnum;
  vnum.value = g->vars_count;
  SLICE_PUSH(g->vars, g->vars_count, g->vars_limit, node);
  return vnum;
}

void funcgraph_init_personal_fields(struct funcgraph *g) {
  g->entry_point = varnum_invalid();
  g->arg_vars = NULL;
  g->arg_vars_count = 0;
  g->return_var = varnum_invalid();
}

void funcgraph_init(struct funcgraph *g) {
  opgraph_init(&g->opg);
  funcgraph_init_personal_fields(g);
}

void funcgraph_init_move(struct funcgraph *g, struct funcgraph *movee) {
  opgraph_init_move(&g->opg, &movee->opg);
  g->entry_point = movee->entry_point;
  g->arg_vars = movee->arg_vars;
  g->arg_vars_count = movee->arg_vars_count;
  g->return_var = movee->return_var;

  funcgraph_init_personal_fields(movee);
}

void funcgraph_destroy(struct funcgraph *g) {
  free(g->arg_vars);
  opgraph_destroy(&g->opg);
  funcgraph_init_personal_fields(g);
}
