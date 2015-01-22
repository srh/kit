#ifndef KIRA_OPGRAPH_H_
#define KIRA_OPGRAPH_H_

#include <stddef.h>
#include <stdint.h>

#include "ast.h"
#include "identmap.h"

struct ast_typeexpr;
struct opnode;
struct varnode;

struct opnum { size_t value; };
struct varnum { size_t value; };

struct funcinfo {
  struct opnum entry_point;
  struct varnum *arg_vars;
  size_t arg_vars_count;
  struct varnum return_var;
};

struct opgraph {
  struct funcinfo fg;

  struct opnode *ops;
  size_t ops_count;
  size_t ops_limit;

  struct varnode *vars;
  size_t vars_count;
  size_t vars_limit;
};

struct varnode {
  struct ast_typeexpr type;

  /* True if the var's data's existence and location is statically
     computable (or provided) as a function of esp, ebp, and eip.  If
     true, begin/end are initialized -- the var exists in a subset of
     the half-open opnum interval [begin, end).  If false, the var
     could still be statically computable as a struct field of another
     var, or for some other reason. */
  int is_known_static;
  /* True only if is_known_static is true. */
  int is_temporary;
  struct opnum begin;
  struct opnum end;
};

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
  /* Primitive addressof operation. */
  OPNODE_ADDRESSOF,
  /* A struct or union field (because I forgot about unions when
     naming this and when writing the code). */
  OPNODE_STRUCTFIELD,
  /* Primitive i32 negation.  Has an overflow output slot.  It's the
     only non-magic unary operator -- which is why we don't have
     OPNODE_UNOP. */
  OPNODE_I32_NEGATE,
  /* Primitive binary operations.  Has an overflow output slot, even
     for binary operations that cannot overflow!? */
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


struct opnum opnum_invalid(void);
int opnum_is_valid(struct opnum);

struct varnum varnum_invalid(void);
int varnum_is_valid(struct varnum);

void opgraph_init(struct opgraph *g);
void opgraph_init_move(struct opgraph *g, struct opgraph *movee);
void opgraph_destroy(struct opgraph *g);

struct varnum opgraph_add_var(struct opgraph *g, struct ast_typeexpr *type);

struct varnode *opgraph_varnode(struct opgraph *g, struct varnum v);
void opgraph_var_starts(struct opgraph *g, struct varnum v, struct opnum start);
void opgraph_var_ends(struct opgraph *g, struct varnum v, struct opnum end);
void opgraph_var_start_temporary(struct opgraph *g, struct varnum v,
                                 struct opnum begin);
int opgraph_var_is_temporary(struct opgraph *g, struct varnum v);
void opgraph_var_end_if_temporary(struct opgraph *g, struct varnum v,
                                  struct opnum end);

struct opnum opgraph_incomplete_nop(struct opgraph *g);
void opgraph_make_nop_complete(struct opgraph *g, struct opnum incomplete_nop,
                               struct opnum target);
struct opnum opgraph_nop(struct opgraph *g, struct opnum target);
struct opnum opgraph_branch(struct opgraph *g,
                            struct varnum condition,
                            struct opnum true_next,
                            struct opnum false_next);
void opgraph_update_branch_else(struct opgraph *g,
                                struct opnum incomplete_op,
                                struct opnum false_target);
struct opnum opgraph_mov(struct opgraph *g,
                         struct varnum src,
                         struct varnum dest);
struct opnum opgraph_mov_from_global(struct opgraph *g,
                                     uint32_t symbol_table_index,
                                     struct varnum dest);
struct opnum opgraph_bool_immediate(struct opgraph *g,
                                    int value,
                                    struct varnum dest);
struct opnum opgraph_i32_immediate(struct opgraph *g,
                                   int32_t value,
                                   struct varnum dest);
struct opnum opgraph_u32_immediate(struct opgraph *g,
                                   uint32_t value,
                                   struct varnum dest);
struct opnum opgraph_return(struct opgraph *g);
struct opnum opgraph_abort(struct opgraph *g);
struct opnum opgraph_call(struct opgraph *g, struct varnum func,
                          struct varnum *args, size_t args_count,
                          struct varnum dest);
struct opnum opgraph_deref(struct opgraph *g, struct varnum pointer,
                           struct varnum pointee);
struct opnum opgraph_addressof(struct opgraph *g, struct varnum pointee,
                               struct varnum pointer);
/* TODO: Oops, we're also using this for unions. */
struct opnum opgraph_structfield(struct opgraph *g, struct varnum operand,
                                 ident_value fieldname,
                                 struct varnum narrowed);
struct opnum opgraph_i32_negate(struct opgraph *g, struct varnum param,
                                struct varnum result, struct varnum overflow);
struct opnum opgraph_binop_intrinsic(struct opgraph *g,
                                     enum ast_binop operator,  /* non-magic binop */
                                     /* lhs and rhs and dest MUST have same type. */
                                     struct varnum lhs, struct varnum rhs,
                                     struct varnum dest,
                                     struct varnum overflow);

/* Returns g->ops_count. */
struct opnum opgraph_future_0(struct opgraph *g);
/* Returns g->ops_count + 1. */
struct opnum opgraph_future_1(struct opgraph *g);

#endif /* KIRA_OPGRAPH_H_ */
