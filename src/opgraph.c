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

void varnode_init(struct varnode *v, struct ast_typeexpr *type) {
  v->is_known_static = 0;
  v->is_temporary = 0;
  ast_typeexpr_init_copy(&v->type, type);
}

void varnode_destroy(struct varnode *v) {
  ast_typeexpr_destroy(&v->type);
}

void funcinfo_init(struct funcinfo *fi) {
  fi->arg_vars = NULL;
  fi->arg_vars_count = 0;
  fi->return_var = varnum_invalid();
}

void funcinfo_init_move(struct funcinfo *fi, struct funcinfo *movee) {
  fi->arg_vars = movee->arg_vars;
  fi->arg_vars_count = movee->arg_vars_count;
  fi->return_var = movee->return_var;

  funcinfo_init(movee);
}

void funcinfo_destroy(struct funcinfo *fi) {
  free(fi->arg_vars);
  funcinfo_init(fi);
}

void opgraph_init(struct opgraph *g) {
  funcinfo_init(&g->fg);

  g->ops = NULL;
  g->ops_count = 0;
  g->ops_limit = 0;

  g->vars = NULL;
  g->vars_count = 0;
  g->vars_limit = 0;
}

void opgraph_init_move(struct opgraph *g, struct opgraph *movee) {
  funcinfo_init_move(&g->fg, &movee->fg);

  g->ops = movee->ops;
  movee->ops = NULL;
  g->ops_count = movee->ops_count;
  movee->ops_count = 0;
  g->ops_limit = movee->ops_limit;
  movee->ops_limit = 0;

  g->vars = movee->vars;
  movee->vars = NULL;
  g->vars_count = movee->vars_count;
  movee->vars_count = 0;
  g->vars_limit = movee->vars_limit;
  movee->vars_limit = 0;
}

void opgraph_destroy(struct opgraph *g) {
  funcinfo_destroy(&g->fg);
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

struct varnode *opgraph_varnode(struct opgraph *g, struct varnum v) {
  CHECK(varnum_is_valid(v));
  CHECK(v.value < g->vars_count);
  return &g->vars[v.value];
}

void opgraph_var_starts(struct opgraph *g, struct varnum v, struct opnum begin) {
  CHECK(opnum_is_valid(begin));
  struct varnode *node = opgraph_varnode(g, v);
  CHECK(!node->is_known_static);

  node->is_known_static = 1;
  node->begin = begin;
  node->end = opnum_invalid();
}

void opgraph_var_start_temporary(struct opgraph *g, struct varnum v,
                                 struct opnum begin) {
  opgraph_var_starts(g, v, begin);
  g->vars[v.value].is_temporary = 1;
}


/* This can also only be called once per varnum (and must be called
   after opgraph_var_starts is called). */
void opgraph_var_ends(struct opgraph *g, struct varnum v, struct opnum end) {
  CHECK(opnum_is_valid(end));
  struct varnode *node = opgraph_varnode(g, v);
  CHECK(node->is_known_static);
  CHECK(!opnum_is_valid(node->end));

  node->end = end;
}

int opgraph_var_is_temporary(struct opgraph *g, struct varnum v) {
  CHECK(varnum_is_valid(v));
  CHECK(v.value < g->vars_count);
  return g->vars[v.value].is_temporary;
}

void opgraph_var_end_if_temporary(struct opgraph *g, struct varnum v,
                                  struct opnum end) {
  if (opgraph_var_is_temporary(g, v)) {
    opgraph_var_ends(g, v, end);
  }
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
  struct opnode node;
  opnode_init_tag(&node, OPNODE_DEREF);
  node.u.deref.pointer = pointer;
  node.u.deref.pointee_var = pointee_var;
  node.u.deref.next = opgraph_future_1(g);
  return opgraph_add(g, node);
}

struct opnum opgraph_addressof(struct opgraph *g, struct varnum pointee,
                               struct varnum pointer) {
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

struct opnum opgraph_entry_point(struct opgraph *g) {
  (void)g;
  struct opnum ret;
  ret.value = 0;
  return ret;
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

void get_nexts(struct opnode *node, struct opnum *nexts[2], size_t *nexts_count_out) {
  switch (node->tag) {
  case OPNODE_NOP:
    nexts[0] = &node->u.nop_next;
    *nexts_count_out = 1;
    break;
  case OPNODE_RETURN:
    *nexts_count_out = 0;
    break;
  case OPNODE_ABORT:
    *nexts_count_out = 0;
    break;
  case OPNODE_BRANCH:
    nexts[0] = &node->u.branch.true_next;
    nexts[1] = &node->u.branch.false_next;
    *nexts_count_out = 2;
    break;
  case OPNODE_MOV:
    nexts[0] = &node->u.mov.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_MOV_FROM_GLOBAL:
    nexts[0] = &node->u.mov_from_global.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_BOOL:
    nexts[0] = &node->u.boole.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_I32:
    nexts[0] = &node->u.i32.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_U32:
    nexts[0] = &node->u.u32.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_CALL:
    nexts[0] = &node->u.call.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_DEREF:
    nexts[0] = &node->u.deref.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_ADDRESSOF:
    nexts[0] = &node->u.addressof.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_STRUCTFIELD:
    nexts[0] = &node->u.structfield.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_I32_NEGATE:
    nexts[0] = &node->u.i32_negate.next;
    *nexts_count_out = 1;
    break;
  case OPNODE_BINOP:
    nexts[0] = &node->u.binop.next;
    *nexts_count_out = 1;
    break;
  default:
    UNREACHABLE();
  }
}
