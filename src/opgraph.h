#ifndef KIRA_OPGRAPH_H_
#define KIRA_OPGRAPH_H_

#include <stddef.h>
#include <stdint.h>

enum ast_binop;
struct ast_typeexpr;
struct opnode;
struct varnode;

struct opgraph {
  /* This is the fg the opgraph belongs to.  It's icky like this but
     it's more convenient than passing the funcgraph and using &g->opg
     everywhere. */
  struct funcgraph *fg;

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


struct funcgraph {
  struct opgraph opg;

  struct opnum entry_point;
  struct varnum *arg_vars;
  size_t arg_vars_count;
  struct varnum return_var;
};

void funcgraph_init(struct funcgraph *g);
void funcgraph_init_move(struct funcgraph *g, struct funcgraph *movee);
void funcgraph_destroy(struct funcgraph *g);


#endif /* KIRA_OPGRAPH_H_ */
