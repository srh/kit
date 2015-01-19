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

struct opnode_branch {
  struct varnum condition;
  struct opnum true_next;
  struct opnum false_next;
};

struct opnode_mov {
  struct varnum src;
  struct varnum dest;
  struct opnum next;
};

enum opnode_tag {
  /* NOP nodes just redirect you to another node -- often we fill in
     the NOP node's "next node" later, when constructing the opgraph. */
  OPNODE_NOP,
  /* Returns from the function -- there is no "next node". */
  OPNODE_RETURN,
  /* Jumps to one of two targets depending on the value. */
  OPNODE_BRANCH,
  /* Copies data from src to dest. */
  OPNODE_MOV,
};

struct opnode {
  enum opnode_tag tag;
  union {
    struct opnum nop_next;
    struct opnode_branch branch;
    struct opnode_mov mov;
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

void opgraph_init(struct opgraph *g, struct funcgraph *fg) {
  g->fg = fg;

  g->ops = NULL;
  g->ops_count = 0;
  g->ops_limit = 0;

  g->vars = NULL;
  g->vars_count = 0;
  g->vars_limit = 0;
}

void opgraph_init_move(struct opgraph *g, struct opgraph *movee,
                       struct funcgraph *new_owner) {
  *g = *movee;
  g->fg = new_owner;
  opgraph_init(movee, NULL);
}

void opgraph_destroy(struct opgraph *g) {
  g->fg = NULL;
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

struct opnum opgraph_add(struct opgraph *g, struct opnode node) {
  struct opnum ret;
  ret.value = g->ops_count;
  SLICE_PUSH(g->ops, g->ops_count, g->ops_limit, node);
  return ret;
}

struct opnum opgraph_nop(struct opgraph *g, struct opnum target) {
  struct opnode node;
  node.tag = OPNODE_NOP;
  node.u.nop_next = target;
  return opgraph_add(g, node);
}

struct opnum opgraph_incomplete_nop(struct opgraph *g) {
  return opgraph_nop(g, opnum_invalid());
}

void opgraph_make_nop_complete(struct opgraph *g, struct opnum incomplete_nop,
                               struct opnum target) {
  CHECK(opnum_is_valid(incomplete_nop) && incomplete_nop.value < g->ops_count);
  CHECK(g->ops[incomplete_nop.value].tag == OPNODE_NOP
        && !opnum_is_valid(g->ops[incomplete_nop.value].u.nop_next));
  CHECK(opnum_is_valid(target));
  g->ops[incomplete_nop.value].u.nop_next = target;
}

struct opnum opgraph_branch(struct opgraph *g,
                            struct varnum condition,
                            struct opnum true_next,
                            struct opnum false_next) {
  struct opnode node;
  node.tag = OPNODE_BRANCH;
  node.u.branch.condition = condition;
  node.u.branch.true_next = true_next;
  node.u.branch.false_next = false_next;
  return opgraph_add(g, node);
}

struct opnum opgraph_mov(struct opgraph *g,
                         struct varnum src,
                         struct varnum dest) {
  struct opnode node;
  node.tag = OPNODE_MOV;
  node.u.mov.src = src;
  node.u.mov.dest = dest;
  node.u.mov.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_return(struct opgraph *g) {
  struct opnode node;
  node.tag = OPNODE_RETURN;
  return opgraph_add(g, node);
}


struct opnum opgraph_future_0(struct opgraph *g) {
  struct opnum ret;
  ret.value = g->ops_count;
  return ret;
}

struct opnum opgraph_future_1(struct opgraph *g) {
  struct opnum ret;
  ret.value = size_add(g->ops_count, 1);
  return ret;
}

void funcgraph_init_personal_fields(struct funcgraph *g) {
  g->entry_point = opnum_invalid();
  g->arg_vars = NULL;
  g->arg_vars_count = 0;
  g->return_var = varnum_invalid();
}

void funcgraph_init(struct funcgraph *g) {
  opgraph_init(&g->opg, g);
  funcgraph_init_personal_fields(g);
}

void funcgraph_init_move(struct funcgraph *g, struct funcgraph *movee) {
  opgraph_init_move(&g->opg, &movee->opg, g);
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
