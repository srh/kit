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

struct opnode_mov_from_global {
  uint32_t symbol_table_index;
  struct varnum dest;
  struct opnum next;
};

struct opnode_bool {
  int value;
  struct varnum dest;
  struct opnum next;
};

struct opnode_i32 {
  int32_t value;
  struct varnum dest;
  struct opnum next;
};

struct opnode_u32 {
  int32_t value;
  struct varnum dest;
  struct opnum next;
};

struct opnode_call {
  struct varnum func;
  struct varnum *args;
  size_t args_count;
  struct varnum dest;
  struct opnum next;
};

struct opnode_deref {
  struct varnum pointer;
  struct varnum pointee_var;
  struct opnum next;
};

struct opnode_addressof {
  struct varnum pointee;
  struct varnum pointer;
  struct opnum next;
};

struct opnode_structfield {
  struct varnum operand;
  ident_value fieldname;
  struct varnum narrowed;
  struct opnum next;
};

struct opnode_i32_negate {
  struct varnum src;
  struct varnum dest;
  struct varnum overflow;
  struct opnum next;
};

struct opnode_binop {
  /* A non-magic binop. */
  enum ast_binop operator;
  /* lhs and rhs and dest have same type. */
  struct varnum lhs;
  struct varnum rhs;
  struct varnum dest;
  struct varnum overflow;
  struct opnum next;
};

enum opnode_tag {
  /* NOP nodes just redirect you to another node -- often we fill in
     the NOP node's "next node" later, when constructing the opgraph. */
  OPNODE_NOP,
  /* Returns from the function -- there is no "next node". */
  OPNODE_RETURN,
  /* Aborts the process immediately. */
  OPNODE_ABORT,
  /* Jumps to one of two targets depending on the value. */
  OPNODE_BRANCH,
  /* Copies data from src to dest. */
  OPNODE_MOV,
  /* Copies data from a symbol to dest. */
  OPNODE_MOV_FROM_GLOBAL,
  /* Immediate bool. */
  OPNODE_BOOL,
  /* Immediate i32. */
  OPNODE_I32,
  /* Immediate u32. */
  OPNODE_U32,
  /* Function call. */
  OPNODE_CALL,
  /* This is unusual because instead of modifying the _contents_ of
     its varnum, pointee_var, it modifies (or specifies) the
     _location_ of its varnum.  TODO: Should we check that the varnum
     has no specified location beforehand? */
  OPNODE_DEREF,
  OPNODE_ADDRESSOF,
  OPNODE_STRUCTFIELD,
  OPNODE_I32_NEGATE,
  OPNODE_BINOP,
};

struct opnode {
  enum opnode_tag tag;
  int is_recursing;
  union {
    struct opnum nop_next;
    struct opnode_branch branch;
    struct opnode_mov mov;
    struct opnode_mov_from_global mov_from_global;
    struct opnode_i32 i32;
    struct opnode_u32 u32;
    struct opnode_bool boole;
    struct opnode_call call;
    struct opnode_deref deref;
    struct opnode_addressof addressof;
    struct opnode_structfield structfield;
    struct opnode_i32_negate i32_negate;
    struct opnode_binop binop;
  } u;
};

void opnode_init_tag(struct opnode *n, enum opnode_tag tag) {
  n->tag = tag;
  n->is_recursing = 0;
}

void opnode_destroy(struct opnode *n) {
  switch (n->tag) {
  case OPNODE_NOP: break;
  case OPNODE_RETURN: break;
  case OPNODE_ABORT: break;
  case OPNODE_BRANCH: break;
  case OPNODE_MOV: break;
  case OPNODE_MOV_FROM_GLOBAL: break;
  case OPNODE_BOOL: break;
  case OPNODE_I32: break;
  case OPNODE_U32: break;
  case OPNODE_CALL:
    free(n->u.call.args);
    break;
  case OPNODE_DEREF: break;
  case OPNODE_ADDRESSOF: break;
  case OPNODE_STRUCTFIELD: break;
  case OPNODE_I32_NEGATE: break;
  case OPNODE_BINOP: break;
  default:
    UNREACHABLE();
  }
  opnode_init_tag(n, (enum opnode_tag)-1);
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
  opnode_init_tag(&node, OPNODE_NOP);
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
  opnode_init_tag(&node, OPNODE_BRANCH);
  node.u.branch.condition = condition;
  node.u.branch.true_next = true_next;
  node.u.branch.false_next = false_next;
  return opgraph_add(g, node);
}

void opgraph_update_branch_else(struct opgraph *g,
                                struct opnum incomplete_op,
                                struct opnum false_target) {
  CHECK(opnum_is_valid(incomplete_op) && incomplete_op.value < g->ops_count);
  CHECK(g->ops[incomplete_op.value].tag == OPNODE_BRANCH
        && !opnum_is_valid(g->ops[incomplete_op.value].u.branch.false_next));
  CHECK(opnum_is_valid(false_target));
  g->ops[incomplete_op.value].u.branch.false_next = false_target;
}

struct opnum opgraph_mov(struct opgraph *g,
                         struct varnum src,
                         struct varnum dest) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_MOV);
  node.u.mov.src = src;
  node.u.mov.dest = dest;
  node.u.mov.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_return(struct opgraph *g) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_RETURN);
  return opgraph_add(g, node);
}

struct opnum opgraph_abort(struct opgraph *g) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_ABORT);
  return opgraph_add(g, node);
}

struct opnum opgraph_call(struct opgraph *g, struct varnum func,
                          struct varnum *args, size_t args_count,
                          struct varnum dest) {
  /* TODO: Check type? */
  struct opnode node;
  opnode_init_tag(&node, OPNODE_CALL);
  node.u.call.func = func;
  node.u.call.args = args;
  node.u.call.args_count = args_count;
  node.u.call.dest = dest;
  node.u.call.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_deref(struct opgraph *g, struct varnum pointer,
                           struct varnum pointee_var) {
  /* TODO: Check type? */
  struct opnode node;
  opnode_init_tag(&node, OPNODE_DEREF);
  node.u.deref.pointer = pointer;
  node.u.deref.pointee_var = pointee_var;
  node.u.deref.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_addressof(struct opgraph *g, struct varnum pointee,
                               struct varnum pointer) {
  /* TODO: Check type? */
  struct opnode node;
  opnode_init_tag(&node, OPNODE_ADDRESSOF);
  node.u.addressof.pointee = pointee;
  node.u.addressof.pointer = pointer;
  node.u.addressof.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_structfield(struct opgraph *g, struct varnum operand,
                                 ident_value fieldname,
                                 struct varnum narrowed) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_STRUCTFIELD);
  node.u.structfield.operand = operand;
  node.u.structfield.fieldname = fieldname;
  node.u.structfield.narrowed = narrowed;
  node.u.structfield.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_i32_negate(struct opgraph *g, struct varnum param,
                                struct varnum result, struct varnum overflow) {
  /* TODO: Check type? */
  struct opnode node;
  opnode_init_tag(&node, OPNODE_I32_NEGATE);
  node.u.i32_negate.src = param;
  node.u.i32_negate.dest = result;
  node.u.i32_negate.overflow = overflow;
  node.u.i32_negate.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_binop_intrinsic(struct opgraph *g,
                                     enum ast_binop operator,  /* non-magic binop */
                                     /* lhs and rhs MUST have same type. */
                                     struct varnum lhs, struct varnum rhs,
                                     struct varnum dest,
                                     struct varnum overflow) {
  /* TODO: Check type? */
  struct opnode node;
  opnode_init_tag(&node, OPNODE_BINOP);
  node.u.binop.operator = operator;
  node.u.binop.lhs = lhs;
  node.u.binop.rhs = rhs;
  node.u.binop.dest = dest;
  node.u.binop.overflow = overflow;
  node.u.binop.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_mov_from_global(struct opgraph *g,
                                     uint32_t symbol_table_index,
                                     struct varnum dest) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_MOV_FROM_GLOBAL);
  node.u.mov_from_global.symbol_table_index = symbol_table_index;
  node.u.mov_from_global.dest = dest;
  node.u.mov_from_global.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_bool_immediate(struct opgraph *g,
                                    int value, /* 1 or 0 */
                                    struct varnum dest) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_BOOL);
  node.u.boole.value = value;
  node.u.boole.dest = dest;
  node.u.boole.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_i32_immediate(struct opgraph *g,
                                   int32_t value,
                                   struct varnum dest) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_I32);
  node.u.i32.value = value;
  node.u.i32.dest = dest;
  node.u.i32.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_u32_immediate(struct opgraph *g,
                                   uint32_t value,
                                   struct varnum dest) {
  struct opnode node;
  opnode_init_tag(&node, OPNODE_U32);
  node.u.u32.value = value;
  node.u.u32.dest = dest;
  node.u.u32.next = opgraph_future_1(g);
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
