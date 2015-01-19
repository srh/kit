#include "build.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "checkstate.h"
#include "databuf.h"
#include "io.h"
#include "opgraph.h"
#include "slice.h"
#include "win/objfile.h"

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
    i++;
  }

  char rbuf[20] = { 0 };

  for (size_t j = 0; j < i; j++) {
    rbuf[j] = buf[i - 1 - j];
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

#define DWORD_SIZE 4

/* X86 WINDOWS */
void kira_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      uint32_t *sizeof_out, uint32_t *alignof_out) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(nt, type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      CRASH("Type name should be found, it was not.\n");
    }
    CHECK(ent->arity.value == ARITY_NO_PARAMLIST);
    if (ent->is_primitive) {
      *sizeof_out = ent->primitive_sizeof;
      *alignof_out = ent->primitive_alignof;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(!deftype->generics.has_type_params);
      kira_sizealignof(nt, &deftype->type, sizeof_out, alignof_out);
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params_count),
                                   &ent)) {
      CRASH("Type app name should be found, it was not.\n");
    }
    CHECK(ent->arity.value == type->u.app.params_count);
    if (ent->is_primitive) {
      *sizeof_out = ent->primitive_sizeof;
      *alignof_out = ent->primitive_alignof;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(deftype->generics.has_type_params
            && deftype->generics.params_count == type->u.app.params_count);
      struct ast_typeexpr substituted;
      /* TODO: Since we're using exprs from a def_instantiation, I
         think the generics have already been replaced. */
      do_replace_generics(&deftype->generics,
                          type->u.app.params,
                          &deftype->type,
                          &substituted);
      kira_sizealignof(nt, &substituted, sizeof_out, alignof_out);
      ast_typeexpr_destroy(&substituted);
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    uint32_t count = 0;
    uint32_t max_alignment = 1;
    for (size_t i = 0, e = type->u.structe.fields_count; i < e; i++) {
      uint32_t size;
      uint32_t alignment;
      kira_sizealignof(nt, &type->u.structe.fields[i].type,
                       &size, &alignment);
      count = uint32_ceil_aligned(count, alignment);
      if (max_alignment < alignment) {
        max_alignment = alignment;
      }
      count = uint32_add(count, size);
    }
    count = uint32_ceil_aligned(count, max_alignment);
    *sizeof_out = count;
    *alignof_out = max_alignment;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    uint32_t max_size = 0;
    uint32_t max_alignment = 1;
    for (size_t i = 0, e = type->u.unione.fields_count; i < e; i++) {
      uint32_t size;
      uint32_t alignment;
      kira_sizealignof(nt, &type->u.unione.fields[i].type,
                       &size, &alignment);
      if (max_size < size) {
        size = max_size;
      }
      if (max_alignment < alignment) {
        max_alignment = alignment;
      }
    }
    uint32_t final_size = uint32_ceil_aligned(max_size, max_alignment);
    *sizeof_out = final_size;
    *alignof_out = max_alignment;
  } break;
  case AST_TYPEEXPR_UNKNOWN:
  default:
    UNREACHABLE();
  }
}

uint32_t kira_sizeof(struct name_table *nt, struct ast_typeexpr *type) {
  uint32_t size;
  uint32_t alignment;
  kira_sizealignof(nt, type, &size, &alignment);
  return size;
}

struct varnum_pair {
  ident_value varname;
  struct varnum varnum;
};

struct label_pair {
  ident_value label_name;
  struct opnum opnum;
};

struct builder_state {
  /* A stack of active variable names and their varnums.  (This gets
     pushed and popped as we traverse scopes of a bracebody.) */
  struct varnum_pair *varnums;
  size_t varnums_count;
  size_t varnums_limit;

  /* An unchanging map of labels to opnums.  (We preallocate
     nop-valued opnodes for each label.) */
  /* TODO: Shouldn't we instead just have a list of (label name, goto
     opnum) pairs?  */
  struct label_pair *labels;
  size_t labels_count;
};

void builder_state_init(struct opgraph *g,
                        struct builder_state *st,
                        ident_value *label_names,
                        size_t label_names_count) {
  st->varnums = NULL;
  st->varnums_count = 0;
  st->varnums_limit = 0;

  struct label_pair *labels = malloc_mul(sizeof(*labels), label_names_count);
  for (size_t i = 0; i < label_names_count; i++) {
    labels[i].label_name = label_names[i];
    labels[i].opnum = opgraph_incomplete_nop(g);
  }
  st->labels = labels;
  st->labels_count = label_names_count;
}

void builder_state_destroy(struct builder_state *st) {
  free(st->labels);
  free(st->varnums);

  st->varnums = NULL;
  st->varnums_count = 0;
  st->varnums_limit = 0;

  st->labels = NULL;
  st->labels_count = 0;
}

int builder_state_try_lookup_varnum(struct builder_state *st,
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
    CHECK(!builder_state_try_lookup_varnum(st, varname, &dummy));
  }
  struct varnum_pair pair;
  pair.varname = varname;
  pair.varnum = varnum;
  SLICE_PUSH(st->varnums, st->varnums_count, st->varnums_limit, pair);
}

struct opnum builder_state_lookup_label_opnum(struct builder_state *st,
                                              ident_value label_name) {
  for (size_t i = 0, e = st->labels_count; i < e; i++) {
    if (st->labels[i].label_name == label_name) {
      return st->labels[i].opnum;
    }
  }
  CRASH("Label name not found.\n");
}

int build_expr(struct checkstate *cs, struct objfile *f,
               struct opgraph *g,
               struct builder_state *st,
               struct ast_expr *a,
               struct varnum *varnum_out) {
  (void)cs, (void)f, (void)g, (void)st, (void)a, (void)varnum_out;
  TODO_IMPLEMENT;
}

/* TODO: Right now we're jumping into implementing build_bracebody so
   that we can see what additional info we'll need (like labels and
   such) before setting the entry_point. */
int build_bracebody(struct checkstate *cs, struct objfile *f,
                    struct opgraph *g,
                    struct builder_state *st,
                    struct ast_bracebody *a) {
  for (size_t i = 0, e = a->statements_count; i < e; i++) {
    struct ast_statement *s = &a->statements[i];
    switch (s->tag) {
    case AST_STATEMENT_EXPR: {
      struct varnum discard;
      if (!build_expr(cs, f, g, st, s->u.expr, &discard)) {
        return 0;
      }
    } break;
    case AST_STATEMENT_RETURN_EXPR: {
      struct varnum var;
      if (!build_expr(cs, f, g, st, s->u.return_expr, &var)) {
        return 0;
      }
      opgraph_mov(g, var, g->fg->return_var);
      opgraph_return(g);
    } break;
    case AST_STATEMENT_VAR: {
      struct ast_var_statement *vs = &s->u.var_statement;
      
    } break;
    case AST_STATEMENT_GOTO: {
      struct opnum target_node = builder_state_lookup_label_opnum(
          st, s->u.goto_statement.target.value);
      opgraph_nop(g, target_node);
    } break;
    case AST_STATEMENT_LABEL: {
      struct opnum s1 = opgraph_future_1(g);
      struct opnum label_target = opgraph_nop(g, s1);
      struct opnum label_node = builder_state_lookup_label_opnum(
          st, s->u.label_statement.label.value);
      opgraph_make_nop_complete(g, label_node, label_target);
    } break;
    case AST_STATEMENT_IFTHEN: {
      TODO_IMPLEMENT;
    } break;
    case AST_STATEMENT_IFTHENELSE: {
      TODO_IMPLEMENT;
    } break;
    default:
      UNREACHABLE();
    }
  }

  return 1;
}

int build_funcgraph(struct checkstate *cs, struct objfile *f,
                    struct ast_expr *lambda_expr,
                    struct funcgraph *out) {
  CHECK(typeexpr_is_func_type(cs->im, &lambda_expr->expr_info.concrete_type));
  CHECK(lambda_expr->tag == AST_EXPR_LAMBDA);

  struct funcgraph g;
  funcgraph_init(&g);

  struct ast_lambda *lambda = &lambda_expr->u.lambda;

  CHECK(lambda->info.lambda_info_valid);

  struct builder_state st;
  builder_state_init(&g.opg, &st, lambda->info.label_names, lambda->info.label_names_count);

  struct ast_typeexpr *return_type
    = expose_func_return_type(cs->im,
                              &lambda_expr->expr_info.concrete_type,
                              size_add(lambda->params_count, 1));

  g.return_var = opgraph_add_var(&g.opg, return_type);
  struct varnum *arg_vars = malloc_mul(sizeof(*arg_vars),
                                       lambda->params_count);
  for (size_t i = 0, e = lambda->params_count; i < e; i++) {
    arg_vars[i] = opgraph_add_var(&g.opg,
                                  &lambda_expr->expr_info.concrete_type.u.app.params[i]);
    builder_state_push_varnum(&st, lambda->params[i].name.value, arg_vars[i]);
  }

  g.arg_vars = arg_vars;
  g.arg_vars_count = lambda->params_count;

  CHECK(!opnum_is_valid(g.entry_point));
  g.entry_point = opgraph_future_0(&g.opg);

  int ret = build_bracebody(cs, f, &g.opg, &st, &lambda->bracebody);
  builder_state_destroy(&st);

  if (ret) {
    funcgraph_init_move(out, &g);
  } else {
    funcgraph_destroy(&g);
  }

  return ret;
}


int build_instantiation(struct checkstate *cs, struct objfile *f,
                        struct def_instantiation *inst) {
  (void)cs;  /* TODO */
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
    struct funcgraph g;
    if (!build_funcgraph(cs, f, &inst->value.u.typechecked_lambda, &g)) {
      return 0;
    }
    TODO_IMPLEMENT;     /* (Generate machine code.) */
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
