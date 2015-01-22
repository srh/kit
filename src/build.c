#include "build.h"

#include <stdio.h>
#include <stdlib.h>

#include "arith.h"
#include "checkstate.h"
#include "databuf.h"
#include "io.h"
#include "opgraph.h"
#include "slice.h"
#include "win/objfile.h"
#include "x86.h"

/* Right now we don't worry about generating multiple objfiles, so we
   just blithely attach a serial number to each name to make them
   unique. */
int generate_kira_name(struct checkstate *cs,
                       const void *name, size_t name_count,
                       void **gen_name_out, size_t *gen_name_count_out) {
  CHECK(cs->kira_name_counter != UINT32_MAX);
  cs->kira_name_counter++;
  uint32_t number_append = cs->kira_name_counter;
  struct databuf b;
  databuf_init(&b);
  databuf_append(&b, "_kira_", 6);
  databuf_append(&b, name, name_count);

  /* I just don't want to lookup the stdarg documentation and
     implement databuf_appendf. */
  char buf[20] = { 0 };
  size_t i = 0;
  CHECK(number_append > 0);
  while (number_append > 0) {
    buf[i] = '0' + (number_append % 10);
    number_append /= 10;
    i = size_add(i, 1);
  }

  char rbuf[20] = { 0 };

  for (size_t j = 0; j < i; j++) {
    rbuf[j] = buf[size_sub(size_sub(i, 1), j)];
  }

  databuf_append(&b, rbuf, i);
  databuf_move_destroy(&b, gen_name_out, gen_name_count_out);
  return 1;
}

int add_def_symbols(struct checkstate *cs, struct objfile *f,
                    struct def_entry *ent) {
  if (ent->is_primitive) {
    return 1;
  }

  const void *name;
  size_t name_count;
  identmap_lookup(cs->im, ent->name, &name, &name_count);

  if (ent->is_extern) {
    if (ent->instantiations_count > 0) {
      /* "extern" defs can't be templatized. */
      CHECK(ent->instantiations_count == 1);

      void *c_name;
      size_t c_name_count;
      if (!objfile_c_symbol_name(name, name_count, &c_name, &c_name_count)) {
        return 0;
      }

      uint32_t symbol_table_index
        = objfile_add_remote_symbol(f, c_name, c_name_count,
                                    typeexpr_is_func_type(cs->im, &ent->type) ?
                                    IS_FUNCTION_YES : IS_FUNCTION_NO);
      free(c_name);

      CHECK(ent->instantiations_count == 1);
      struct def_instantiation *inst = ent->instantiations[0];
      CHECK(!inst->symbol_table_index_computed);
      inst->symbol_table_index_computed = 1;
      inst->symbol_table_index = symbol_table_index;
    }

    return 1;
  }

  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations[i];

    void *gen_name;
    size_t gen_name_count;
    if (!generate_kira_name(cs, name, name_count,
                            &gen_name, &gen_name_count)) {
      return 0;
    }

    /* We later overwrite the symbol's value (we write zero here). */
    /* No defs are put in a read-only section... for now. */
    uint32_t symbol_table_index
      = objfile_add_local_symbol(f, gen_name, gen_name_count,
                                 0 /* We'll overwrite the value later. */,
                                 typeexpr_is_func_type(cs->im, &ent->type) ?
                                 SECTION_TEXT : SECTION_DATA,
                                 IS_STATIC_NO);
    free(gen_name);

    CHECK(!inst->symbol_table_index_computed);
    inst->symbol_table_index_computed = 1;
    inst->symbol_table_index = symbol_table_index;
  }

  return 1;
}

struct varnum_pair {
  ident_value varname;
  struct varnum varnum;
};

struct label_pair {
  ident_value label_name;
  struct opnum opnum;
};

struct goto_pair {
  /* Which nop should be made to point at the target. */
  struct opnum opnum;
  ident_value target_name;
};

struct builder_state {
  /* A stack of active variable names and their varnums.  (This gets
     pushed and popped as we traverse scopes of a bracebody.) */
  struct varnum_pair *varnums;
  size_t varnums_count;
  size_t varnums_limit;

  /* An map of labels to opnums. */
  struct label_pair *labels;
  size_t labels_count;
  size_t labels_limit;

  struct goto_pair *gotos;
  size_t gotos_count;
  size_t gotos_limit;
};

void builder_state_init(struct builder_state *st) {
  st->varnums = NULL;
  st->varnums_count = 0;
  st->varnums_limit = 0;

  st->labels = NULL;
  st->labels_count = 0;
  st->labels_limit = 0;

  st->gotos = NULL;
  st->gotos_count = 0;
  st->gotos_limit = 0;
}

void builder_state_destroy(struct builder_state *st) {
  free(st->gotos);
  free(st->labels);
  free(st->varnums);

  st->varnums = NULL;
  st->varnums_count = 0;
  st->varnums_limit = 0;

  st->labels = NULL;
  st->labels_count = 0;
  st->labels_limit = 0;

  st->gotos = NULL;
  st->gotos_count = 0;
  st->gotos_limit = 0;
}

int builder_state_lookup_varnum(struct builder_state *st,
                                ident_value varname,
                                struct varnum *varnum_out) {
  for (size_t i = 0, e = st->varnums_count; i < e; i++) {
    if (st->varnums[i].varname == varname) {
      *varnum_out = st->varnums[i].varnum;
      return 1;
    }
  }
  return 0;
}

void builder_state_push_varnum(struct builder_state *st,
                               ident_value varname,
                               struct varnum varnum) {
  {
    struct varnum dummy;
    CHECK(!builder_state_lookup_varnum(st, varname, &dummy));
  }
  struct varnum_pair pair;
  pair.varname = varname;
  pair.varnum = varnum;
  SLICE_PUSH(st->varnums, st->varnums_count, st->varnums_limit, pair);
}

struct varnum builder_state_pop_varnum(struct builder_state *st) {
  CHECK(st->varnums_count > 0);
  st->varnums_count--;
  return st->varnums[st->varnums_count].varnum;
}

void builder_state_note_label_target(struct builder_state *st,
                                     ident_value label_name,
                                     struct opnum target) {
  struct label_pair pair;
  pair.label_name = label_name;
  pair.opnum = target;
  SLICE_PUSH(st->labels, st->labels_count, st->labels_limit, pair);
}

void builder_state_note_goto(struct builder_state *st,
                             struct opnum incomplete_nop,
                             ident_value target) {
  struct goto_pair pair;
  pair.opnum = incomplete_nop;
  pair.target_name = target;
  SLICE_PUSH(st->gotos, st->gotos_count, st->gotos_limit, pair);
}

struct opnum builder_state_lookup_label(struct builder_state *st,
                                        ident_value label_name) {
  for (size_t i = 0, e = st->labels_count; i < e; i++) {
    if (st->labels[i].label_name == label_name) {
      return st->labels[i].opnum;
    }
  }
  UNREACHABLE();
}

void connect_gotos_and_labels(struct builder_state *st,
                              struct opgraph *g) {
  for (size_t i = 0, e = st->gotos_count; i < e; i++) {
    struct opnum target
      = builder_state_lookup_label(st, st->gotos[i].target_name);

    opgraph_make_nop_complete(g, st->gotos[i].opnum, target);
  }
}

int build_numeric_literal(struct checkstate *cs,
                          struct opgraph *g,
                          struct ast_numeric_literal *a,
                          struct varnum *varnum_out) {
  struct ast_typeexpr type;
  numeric_literal_type(cs->im, a, &type);
  struct varnum v = opgraph_add_var(g, &type);
  ast_typeexpr_destroy(&type);
  opgraph_var_start_temporary(g, v, opgraph_future_0(g));

  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED: {
    int32_t value;
    if (!numeric_literal_to_i32(a->digits, a->digits_count, &value)) {
      return 0;
    }
    opgraph_i32_immediate(g, value, v);
  } break;
  case AST_NUMERIC_TYPE_UNSIGNED: {
    uint32_t value;
    if (!numeric_literal_to_u32(a->digits, a->digits_count, &value)) {
      return 0;
    }
    opgraph_u32_immediate(g, value, v);
  } break;
  default:
    UNREACHABLE();
  }

  *varnum_out = v;
  return 1;
}

void build_conditional_abort(struct opgraph *g,
                             struct varnum condition) {
  struct opnum then_fut = opgraph_future_1(g);
  struct opnum node = opgraph_branch(g, condition,
                                     then_fut,
                                     opnum_invalid());
  opgraph_abort(g);
  opgraph_update_branch_else(g, node, opgraph_future_0(g));
}

int build_expr(struct checkstate *cs,
               struct opgraph *g,
               struct builder_state *st,
               struct ast_expr *a,
               int lvalue,
               struct varnum *varnum_out);

struct varnum add_bool_var(struct identmap *im, struct opgraph *g) {
  struct ast_typeexpr bool_type;
  init_name_type(&bool_type,
                 identmap_intern_c_str(im, BOOLEAN_STANDIN_TYPE_NAME));
  struct varnum ret = opgraph_add_var(g, &bool_type);
  ast_typeexpr_destroy(&bool_type);
  return ret;
}

void end_if_temporary_0(struct opgraph *g, struct varnum v) {
  opgraph_var_end_if_temporary(g, v, opgraph_future_0(g));
}

void start_temporary_0(struct opgraph *g, struct varnum v) {
  opgraph_var_start_temporary(g, v, opgraph_future_0(g));
}

int build_binop_expr(struct checkstate *cs,
                     struct opgraph *g,
                     struct builder_state *st,
                     struct ast_expr *a,  /* a->tag == AST_EXPR_BINOP */
                     int lvalue,
                     struct varnum *varnum_out) {
  struct ast_binop_expr *be = &a->u.binop_expr;
  switch (be->operator) {
  case AST_BINOP_ASSIGN: {
    struct varnum lhs;
    if (!build_expr(cs, g, st, be->lhs, 1, &lhs)) {
      return 0;
    }
    struct varnum rhs;
    if (!build_expr(cs, g, st, be->rhs, 0, &rhs)) {
      return 0;
    }
    opgraph_mov(g, rhs, lhs);
    end_if_temporary_0(g, rhs);
    *varnum_out = lhs;
    return 1;
  } break;
  case AST_BINOP_LOGICAL_OR: {
    CHECK(!lvalue);
    struct varnum lhs;
    if (!build_expr(cs, g, st, be->lhs, 0, &lhs)) {
      return 0;
    }
    struct varnum result = add_bool_var(cs->im, g);

    struct opnum true_fut = opgraph_future_1(g);
    struct opnum branch = opgraph_branch(g, lhs,
                                         true_fut,
                                         opnum_invalid());
    end_if_temporary_0(g, lhs);

    start_temporary_0(g, result);
    opgraph_bool_immediate(g, 1, result);
    struct opnum true_nop = opgraph_incomplete_nop(g);
    opgraph_update_branch_else(g, branch,
                               opgraph_future_0(g));

    struct varnum rhs;
    if (!build_expr(cs, g, st, be->rhs, 0, &rhs)) {
      return 0;
    }
    opgraph_mov(g, rhs, result);
    end_if_temporary_0(g, rhs);

    opgraph_make_nop_complete(g, true_nop, opgraph_future_0(g));
    *varnum_out = result;
    return 1;
  } break;
  case AST_BINOP_LOGICAL_AND: {
    CHECK(!lvalue);
    struct varnum lhs;
    if (!build_expr(cs, g, st, be->lhs, 0, &lhs)) {
      return 0;
    }
    struct varnum result = add_bool_var(cs->im, g);

    struct opnum true_fut = opgraph_future_1(g);
    struct opnum branch = opgraph_branch(g, lhs,
                                         true_fut,
                                         opnum_invalid());
    end_if_temporary_0(g, lhs);

    struct varnum rhs;
    if (!build_expr(cs, g, st, be->rhs, 0, &rhs)) {
      return 0;
    }

    start_temporary_0(g, result);
    opgraph_mov(g, rhs, result);
    end_if_temporary_0(g, rhs);

    struct opnum true_nop = opgraph_incomplete_nop(g);

    opgraph_update_branch_else(g, branch,
                               opgraph_future_0(g));

    opgraph_bool_immediate(g, 0, result);

    opgraph_make_nop_complete(g, true_nop, opgraph_future_0(g));
    *varnum_out = result;
    return 1;
  } break;
  case AST_BINOP_ADD:  /* fall-through */
  case AST_BINOP_SUB:  /* fall-through */
  case AST_BINOP_MUL:  /* fall-through */
  case AST_BINOP_DIV:  /* fall-through */
  case AST_BINOP_MOD:  /* fall-through */
  case AST_BINOP_LT:  /* fall-through */
  case AST_BINOP_LE:  /* fall-through */
  case AST_BINOP_GT:  /* fall-through */
  case AST_BINOP_GE:  /* fall-through */
  case AST_BINOP_EQ:  /* fall-through */
  case AST_BINOP_NE:  /* fall-through */
  case AST_BINOP_BIT_XOR:  /* fall-through */
  case AST_BINOP_BIT_OR:  /* fall-through */
  case AST_BINOP_BIT_AND:  /* fall-through */
  case AST_BINOP_BIT_LEFTSHIFT:  /* fall-through */
  case AST_BINOP_BIT_RIGHTSHIFT: {
    struct varnum lhs;
    if (!build_expr(cs, g, st, be->lhs, 0, &lhs)) {
      return 0;
    }
    struct varnum rhs;
    if (!build_expr(cs, g, st, be->rhs, 0, &rhs)) {
      return 0;
    }
    struct varnum dest = opgraph_add_var(g, &a->expr_info.concrete_type);
    struct varnum overflow = add_bool_var(cs->im, g);

    start_temporary_0(g, dest);
    start_temporary_0(g, overflow);
    opgraph_binop_intrinsic(g, be->operator, lhs, rhs, dest, overflow);
    end_if_temporary_0(g, lhs);
    end_if_temporary_0(g, rhs);
    build_conditional_abort(g, overflow);
    end_if_temporary_0(g, overflow);

    *varnum_out = dest;
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int build_unop_expr(struct checkstate *cs,
                    struct opgraph *g,
                    struct builder_state *st,
                    struct ast_expr *a,  /* a->tag == AST_EXPR_UNOP */
                    int lvalue,
                    struct varnum *varnum_out) {
  struct ast_unop_expr *ue = &a->u.unop_expr;
  switch (ue->operator) {
  case AST_UNOP_DEREFERENCE: {
    struct varnum rhs;
    if (!build_expr(cs, g, st, ue->rhs, 0, &rhs)) {
      return 0;
    }
    struct varnum deref = opgraph_add_var(g, &a->expr_info.concrete_type);
    opgraph_deref(g, rhs, deref);
    end_if_temporary_0(g, rhs);
    *varnum_out = deref;
    return 1;
  } break;
  case AST_UNOP_ADDRESSOF: {
    CHECK(!lvalue);
    struct varnum rhs;
    if (!build_expr(cs, g, st, ue->rhs, 1, &rhs)) {
      return 0;
    }
    struct varnum ref = opgraph_add_var(g, &a->expr_info.concrete_type);
    opgraph_addressof(g, rhs, ref);
    CHECK(!opgraph_var_is_temporary(g, rhs));
    *varnum_out = ref;
    return 1;
  } break;
  case AST_UNOP_NEGATE: {
    CHECK(!lvalue);
    struct varnum rhs;
    if (!build_expr(cs, g, st, ue->rhs, 0, &rhs)) {
      return 0;
    }
    struct varnum result = opgraph_add_var(g, &a->expr_info.concrete_type);
    struct varnum overflow = add_bool_var(cs->im, g);
    start_temporary_0(g, result);
    start_temporary_0(g, overflow);

    opgraph_i32_negate(g, rhs, result, overflow);
    end_if_temporary_0(g, rhs);

    build_conditional_abort(g, overflow);
    end_if_temporary_0(g, overflow);
    *varnum_out = result;
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int build_expr(struct checkstate *cs,
               struct opgraph *g,
               struct builder_state *st,
               struct ast_expr *a,
               int lvalue,
               struct varnum *varnum_out) {
  switch (a->tag) {
  case AST_EXPR_NAME: {
    struct ast_name_expr *ne = &a->u.name;
    CHECK(ne->info.info_valid);
    if (ne->info.inst_or_null) {
      CHECK(!lvalue);
      struct def_instantiation *inst = ne->info.inst_or_null;
      struct varnum v = opgraph_add_var(g, &inst->type);
      /* TODO: We need to make functions move the pointer, not value, _somewhere_. */
      opgraph_mov_from_global(g, inst->symbol_table_index, v);
      *varnum_out = v;
      return 1;
    } else {
      return builder_state_lookup_varnum(st, ne->ident.value, varnum_out);
    }
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    CHECK(!lvalue);
    return build_numeric_literal(cs, g, &a->u.numeric_literal, varnum_out);
  } break;
  case AST_EXPR_FUNCALL: {
    CHECK(!lvalue);
    struct ast_funcall *fe = &a->u.funcall;
    struct varnum func;
    if (!build_expr(cs, g, st, fe->func, 0, &func)) {
      return 0;
    }
    size_t args_count = fe->args_count;
    struct varnum *args = malloc_mul(sizeof(*args), args_count);
    for (size_t i = 0; i < args_count; i++) {
      if (!build_expr(cs, g, st, &fe->args[i], 0, &args[i])) {
        free(args);
        return 0;
      }
    }
    struct varnum result
      = opgraph_add_var(g, expose_func_return_type(cs->im,
                                                   &a->expr_info.concrete_type,
                                                   size_add(args_count, 1)));
    start_temporary_0(g, result);
    opgraph_call(g, func, args, args_count, result);
    end_if_temporary_0(g, func);
    /* TODO: Copy args or something, it's ghetto to assume it's still
       valid after moving the array into opgraph_call. */
    for (size_t i = 0; i < args_count; i++) {
      end_if_temporary_0(g, args[i]);
    }
    *varnum_out = result;
    return 1;
  } break;
  case AST_EXPR_UNOP: {
    return build_unop_expr(cs, g, st, a, lvalue, varnum_out);
  } break;
  case AST_EXPR_BINOP: {
    return build_binop_expr(cs, g, st, a, lvalue, varnum_out);
  } break;
  case AST_EXPR_LAMBDA: {
    /* TODO: Support inline lambdas. */
    ERR_DBG("Compiling inline lambdas doesn't work right now (but it did type-check!)\n");
    return 0;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    struct ast_local_field_access *lfa = &a->u.local_field_access;
    struct varnum operand;
    if (!build_expr(cs, g, st, lfa->lhs, lvalue, &operand)) {
      return 0;
    }

    struct varnum narrowed = opgraph_add_var(g, &a->expr_info.concrete_type);
    if (opgraph_var_is_temporary(g, operand)) {
      start_temporary_0(g, narrowed);
    }
    opgraph_structfield(g, operand, lfa->fieldname.value, narrowed);
    end_if_temporary_0(g, operand);
    *varnum_out = narrowed;
    return 1;
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    struct ast_deref_field_access *dfa = &a->u.deref_field_access;
    struct varnum operand;
    if (!build_expr(cs, g, st, dfa->lhs, lvalue, &operand)) {
      return 0;
    }

    struct ast_typeexpr *ptr_target;
    if (!view_ptr_target(cs->im, &dfa->lhs->expr_info.concrete_type,
                         &ptr_target)) {
      CRASH("Expected pointer type when building deref field access expr.\n");
    }

    struct varnum derefed = opgraph_add_var(g, ptr_target);
    opgraph_deref(g, operand, derefed);
    end_if_temporary_0(g, operand);

    struct varnum narrowed = opgraph_add_var(g, &a->expr_info.concrete_type);
    opgraph_structfield(g, derefed, dfa->fieldname.value, narrowed);
    *varnum_out = narrowed;
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int build_bracebody(struct checkstate *cs,
                    struct opgraph *g,
                    struct builder_state *st,
                    struct ast_bracebody *a) {
  size_t vars_pushed = 0;
  for (size_t i = 0, e = a->statements_count; i < e; i++) {
    struct ast_statement *s = &a->statements[i];
    switch (s->tag) {
    case AST_STATEMENT_EXPR: {
      struct varnum result;
      if (!build_expr(cs, g, st, s->u.expr, 0, &result)) {
        return 0;
      }
      end_if_temporary_0(g, result);
    } break;
    case AST_STATEMENT_RETURN_EXPR: {
      struct varnum var;
      if (!build_expr(cs, g, st, s->u.return_expr, 0, &var)) {
        return 0;
      }
      opgraph_mov(g, var, g->fg.return_var);
      end_if_temporary_0(g, var);
      opgraph_return(g);
    } break;
    case AST_STATEMENT_VAR: {
      struct ast_var_statement *vs = &s->u.var_statement;
      struct varnum rhs_result;
      if (!build_expr(cs, g, st, vs->rhs, 0, &rhs_result)) {
        return 0;
      }

      CHECK(vs->info.var_statement_info_valid);
      struct varnum varnum = opgraph_add_var(g, &vs->info.concrete_type);

      /* We call opgraph_var_ends when we pop the var from the
         builder_state. */
      opgraph_var_starts(g, varnum, opgraph_future_0(g));

      builder_state_push_varnum(st, vs->decl.name.value, varnum);
      vars_pushed = size_add(vars_pushed, 1);

      opgraph_mov(g, rhs_result, varnum);
      end_if_temporary_0(g, rhs_result);
    } break;
    case AST_STATEMENT_GOTO: {
      struct opnum nop = opgraph_incomplete_nop(g);
      builder_state_note_goto(st, nop, s->u.goto_statement.target.value);
    } break;
    case AST_STATEMENT_LABEL: {
      builder_state_note_label_target(st, s->u.label_statement.label.value,
                                      opgraph_future_0(g));
    } break;
    case AST_STATEMENT_IFTHEN: {
      struct ast_ifthen_statement *its = &s->u.ifthen_statement;
      struct varnum condition_result;
      if (!build_expr(cs, g, st, its->condition, 0, &condition_result)) {
        return 0;
      }
      struct opnum fut1 = opgraph_future_1(g);
      struct opnum branch_opnum = opgraph_branch(g, condition_result,
                                                 fut1,
                                                 opnum_invalid());
      end_if_temporary_0(g, condition_result);
      if (!build_bracebody(cs, g, st, &its->thenbody)) {
        return 0;
      }
      struct opnum after_thenbody = opgraph_future_0(g);
      opgraph_update_branch_else(g, branch_opnum, after_thenbody);
    } break;
    case AST_STATEMENT_IFTHENELSE: {
      struct ast_ifthenelse_statement *ites = &s->u.ifthenelse_statement;
      struct varnum condition_result;
      if (!build_expr(cs, g, st, ites->condition, 0, &condition_result)) {
        return 0;
      }
      struct opnum fut1 = opgraph_future_1(g);
      struct opnum branch_opnum = opgraph_branch(g, condition_result,
                                                 fut1,
                                                 opnum_invalid());
      end_if_temporary_0(g, condition_result);
      if (!build_bracebody(cs, g, st, &ites->thenbody)) {
        return 0;
      }
      struct opnum thenbody_nop = opgraph_incomplete_nop(g);
      struct opnum after_thenbody = opgraph_future_0(g);
      opgraph_update_branch_else(g, branch_opnum, after_thenbody);
      if (!build_bracebody(cs, g, st, &ites->elsebody)) {
        return 0;
      }
      struct opnum after_elsebody = opgraph_future_0(g);
      opgraph_make_nop_complete(g, thenbody_nop, after_elsebody);
    } break;
    default:
      UNREACHABLE();
    }
  }

  for (size_t i = 0; i < vars_pushed; i++) {
    struct varnum v = builder_state_pop_varnum(st);
    opgraph_var_ends(g, v, opgraph_future_0(g));
  }

  return 1;
}

int build_opgraph(struct checkstate *cs,
                  struct ast_expr *lambda_expr,
                  struct opgraph *out) {
  CHECK(typeexpr_is_func_type(cs->im, &lambda_expr->expr_info.concrete_type));
  CHECK(lambda_expr->tag == AST_EXPR_LAMBDA);

  struct opgraph g;
  opgraph_init(&g);

  struct ast_lambda *lambda = &lambda_expr->u.lambda;

  CHECK(lambda->info.lambda_info_valid);

  struct builder_state st;
  builder_state_init(&st);

  struct ast_typeexpr *return_type
    = expose_func_return_type(cs->im,
                              &lambda_expr->expr_info.concrete_type,
                              size_add(lambda->params_count, 1));

  g.fg.return_var = opgraph_add_var(&g, return_type);
  struct varnum *arg_vars = malloc_mul(sizeof(*arg_vars),
                                       lambda->params_count);
  for (size_t i = 0, e = lambda->params_count; i < e; i++) {
    arg_vars[i] = opgraph_add_var(&g,
                                  &lambda_expr->expr_info.concrete_type.u.app.params[i]);
    builder_state_push_varnum(&st, lambda->params[i].name.value, arg_vars[i]);
  }

  g.fg.arg_vars = arg_vars;
  g.fg.arg_vars_count = lambda->params_count;

  CHECK(!opnum_is_valid(g.fg.entry_point));
  g.fg.entry_point = opgraph_future_0(&g);

  int ret = build_bracebody(cs, &g, &st, &lambda->bracebody);

  connect_gotos_and_labels(&st, &g);

  builder_state_destroy(&st);

  if (ret) {
    opgraph_init_move(out, &g);
  } else {
    opgraph_destroy(&g);
  }

  return ret;
}


int build_instantiation(struct checkstate *cs, struct objfile *f,
                        struct def_instantiation *inst) {
  switch (inst->value.tag) {
  case STATIC_VALUE_I32: {
    STATIC_CHECK(sizeof(inst->value.u.i32_value) == 4);
    CHECK(inst->symbol_table_index_computed);
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, inst->symbol_table_index,
                             objfile_section_size(objfile_data(f)));
    objfile_section_append_raw(objfile_data(f),
                               &inst->value.u.i32_value,
                               sizeof(inst->value.u.i32_value));
    return 1;
  } break;
  case STATIC_VALUE_U32: {
    STATIC_CHECK(sizeof(inst->value.u.u32_value) == 4);
    CHECK(inst->symbol_table_index_computed);
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, inst->symbol_table_index,
                             objfile_section_size(objfile_data(f)));
    objfile_section_append_raw(objfile_data(f),
                               &inst->value.u.u32_value,
                               sizeof(inst->value.u.u32_value));
    return 1;
  } break;
  case STATIC_VALUE_LAMBDA: {
    struct opgraph g;
    if (!build_opgraph(cs, &inst->value.u.typechecked_lambda, &g)) {
      return 0;
    }

    if (!gen_x86_function(cs, f, &g)) {
      return 0;
    }
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int build_def(struct checkstate *cs, struct objfile *f,
              struct def_entry *ent) {
  if (ent->is_primitive) {
    return 1;
  }

  if (ent->is_extern) {
    return 1;
  }

  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    if (!build_instantiation(cs, f, ent->instantiations[i])) {
      return 0;
    }
  }

  return 1;
}

int build_module(struct identmap *im, module_loader *loader,
                 ident_value name) {
  int ret = 0;
  struct checkstate cs;
  checkstate_init(&cs, im);

  if (!chase_modules_and_typecheck(&cs, loader, name)) {
    goto cleanup_checkstate;
  }

  struct objfile *objfile = NULL;
  objfile_alloc(&objfile);

  for (size_t i = 0, e = cs.nt.defs_count; i < e; i++) {
    if (!add_def_symbols(&cs, objfile, cs.nt.defs[i])) {
      goto cleanup_objfile;
    }
  }

  for (size_t i = 0, e = cs.nt.defs_count; i < e; i++) {
    if (!build_def(&cs, objfile, cs.nt.defs[i])) {
      goto cleanup_objfile;
    }
  }

  struct databuf *databuf = NULL;
  objfile_flatten(objfile, &databuf);

  void *buf;
  size_t buf_size;
  databuf_move_destroy(databuf, &buf, &buf_size);
  free(databuf);

  const void *name_buf;
  size_t name_count;
  identmap_lookup(im, name, &name_buf, &name_count);
  char *path;
  size_t path_count;
  alloc_half_strcat(name_buf, name_count, ".obj",
                    &path, &path_count);
  if (!write_file(path, buf, buf_size)) {
    goto cleanup_path_and_buf;
  }

  ret = 1;
 cleanup_path_and_buf:
  free(path);
  free(buf);
 cleanup_objfile:
  objfile_free(&objfile);
 cleanup_checkstate:
  checkstate_destroy(&cs);
  return ret;
}
