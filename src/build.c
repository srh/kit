#include "build.h"

#include <stdio.h>
#include <stdlib.h>

#include "arith.h"
#include "checkstate.h"
#include "databuf.h"
#include "io.h"
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

enum loc_tag {
  LOC_EBP_OFFSET,
};

struct loc {
  enum loc_tag tag;
  union {
    uint32_t ebp_offset;
  } u;
};

int loc_equal(struct loc a, struct loc b) {
  if (a.tag != b.tag) {
    return 0;
  }
  switch (a.tag) {
  case LOC_EBP_OFFSET:
    return a.u.ebp_offset == b.u.ebp_offset;
  default:
    UNREACHABLE();
  }
}

struct vardata {
  ident_value name;
  size_t number;

  /* A non-owned reference to the type. */
  struct ast_typeexpr *concrete_type;
  /* The size of the type. */
  uint32_t size;
  struct loc loc;
};

void vardata_init(struct vardata *vd,
                  ident_value name,
                  size_t var_number,
                  struct ast_typeexpr *concrete_type,
                  uint32_t size,
                  struct loc initial_loc) {
  vd->name = name;
  vd->number = var_number;
  vd->concrete_type = concrete_type;
  vd->size = size;
  vd->loc = initial_loc;
}

void vardata_init_copy(struct vardata *vd,
                       struct vardata *c) {
  *vd = *c;
}

void vardata_destroy(struct vardata *vd) {
  vd->name = IDENT_VALUE_INVALID;
  vd->concrete_type = NULL;
  vd->size = 0;
  vd->loc.tag = (enum loc_tag)-1;
}

struct labeldata {
  ident_value labelname;

  struct vardata *captured_vardata;
  size_t captured_vardata_count;
  size_t captured_vardata_limit;

  int label_found;
};

void labeldata_init(struct labeldata *ld, ident_value labelname,
                    struct vardata *vardata, size_t vardata_count,
                    int label_found) {
  ld->labelname = labelname;

  struct vardata *copy = malloc_mul(sizeof(*copy), vardata_count);
  for (size_t i = 0; i < vardata_count; i++) {
    vardata_init_copy(copy, vardata);
  }

  ld->captured_vardata = copy;
  ld->captured_vardata_count = vardata_count;
  ld->captured_vardata_limit = vardata_count;
  ld->label_found = label_found;
}

void labeldata_destroy(struct labeldata *ld) {
  ld->labelname = IDENT_VALUE_INVALID;
  SLICE_FREE(ld->captured_vardata, ld->captured_vardata_count, vardata_destroy);
  ld->captured_vardata_limit = 0;
  ld->label_found = 0;
}

struct frame {
  size_t var_number;

  struct vardata *vardata;
  size_t vardata_count;
  size_t vardata_limit;

  struct labeldata *labeldata;
  size_t labeldata_count;
  size_t labeldata_limit;
};

void frame_init(struct frame *h) {
  h->var_number = 0;

  h->vardata = NULL;
  h->vardata_count = 0;
  h->vardata_limit = 0;

  h->labeldata = NULL;
  h->labeldata_count = 0;
  h->labeldata_limit = 0;
}

void frame_destroy(struct frame *h) {
  SLICE_FREE(h->vardata, h->vardata_count, vardata_destroy);
  h->vardata_limit = 0;
  SLICE_FREE(h->labeldata, h->labeldata_count, labeldata_destroy);
  h->labeldata_limit = 0;
}

struct labeldata *frame_try_find_labeldata(struct frame *h, ident_value labelname) {
  for (size_t i = 0, e = h->labeldata_count; i < e; i++) {
    if (h->labeldata[i].labelname == labelname) {
      return &h->labeldata[i];
    }
  }
  return NULL;
}

/* X86 and maybe WINDOWS-specific calling convention stuff. */
void note_param_locations(struct checkstate *cs, struct frame *h, struct ast_expr *expr) {
  struct ast_typeexpr *type = &expr->expr_info.concrete_type;
  size_t args_count = expr->u.lambda.params_count;
  struct ast_typeexpr *return_type
    = expose_func_return_type(cs->im, type, size_add(args_count, 1));

  uint32_t return_type_size = kira_sizeof(&cs->nt, return_type);

  int32_t offset = (2 + (return_type_size > 8)) * DWORD_SIZE;

  for (size_t i = 0, e = expr->u.lambda.params_count; i < e; i++) {
    struct ast_typeexpr *param_type = &type->u.app.params[i];
    struct loc loc;
    loc.tag = LOC_EBP_OFFSET;
    loc.u.ebp_offset = offset;

    uint32_t size = kira_sizeof(&cs->nt, param_type);

    size_t var_number = h->var_number;
    h->var_number++;

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 var_number, param_type, size, loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    offset = int32_add(offset,
                       uint32_to_int32(uint32_ceil_aligned(size, DWORD_SIZE)));
  }
}

void gen_expr(struct checkstate *cs, struct objfile *f,
              struct frame *h, struct ast_expr *expr,
              struct loc *return_val_out) {
  (void)cs, (void)f, (void)h, (void)expr, (void)return_val_out;
  /* TODO: Implement. */
}

void gen_return(struct checkstate *cs, struct objfile *f,
                struct frame *h, struct loc return_value) {
  (void)cs, (void)f, (void)h, (void)return_value;
  /* TODO: Implement. */
}

int lookup_vardata(struct vardata *vd, size_t vd_count, size_t var_number,
                   size_t *index_out) {
  for (size_t i = 0; i < vd_count; i++) {
    if (vd->number == var_number) {
      *index_out = i;
      return 1;
    }
  }
  return 0;
}

struct movpair {
  size_t number;
  struct loc src;
  struct loc dest;
};

/* When you see the label, this must be called _before_ setting
   ld->label_found! */
void mov_and_add_vardata(struct labeldata *ld,
                         struct frame *h, struct objfile *f) {
  struct movpair *movpack = NULL;
  size_t movpack_count = 0;
  size_t movpack_limit = 0;

  for (size_t i = 0, e = ld->captured_vardata_count; i < e; i++) {
    size_t hi;
    if (lookup_vardata(h->vardata, h->vardata_count, ld->captured_vardata[i].number, &hi)) {
      struct vardata *datum = &h->vardata[hi];
      /* The frame has the given var for the label, so move it to the right place. */
      if (!loc_equal(datum->loc, ld->captured_vardata[i].loc)) {
        struct movpair mp;
        mp.number = datum->number;
        mp.src = datum->loc;
        mp.dest = ld->captured_vardata[i].loc;
        SLICE_PUSH(movpack, movpack_count, movpack_limit, mp);
      } else {
        /* Do nothing -- it's already in the right place. */
      }
    } else {
      /* The frame doesn't have the given var for the label. */
    }
  }

  /* Now add the current frame's vardata to the info. */
  if (!ld->label_found) {
    for (size_t i = 0, e = h->vardata_count; i < e; i++) {
      size_t li;
      if (lookup_vardata(ld->captured_vardata, ld->captured_vardata_count,
                         h->vardata[i].number, &li)) {
        /* The label already has a location for this variable -- it's
           part of the movpack if necessary. */
      } else {
        /* Capture our frame's vardata.  The label adopts our frame's
           variable location as its own. */
        struct vardata vd;
        vardata_init_copy(&vd, &h->vardata[i]);
        SLICE_PUSH(ld->captured_vardata, ld->captured_vardata_count,
                   ld->captured_vardata_limit, vd);
      }
    }
  }

  /* TODO: Actually use the movpack to do a bunch of movs (instead of just updating h's locations). */
  for (size_t i = 0; i < movpack_count; i++) {
    size_t hi;
    int success = lookup_vardata(h->vardata, h->vardata_count, movpack[i].number, &hi);
    CHECK(success);
    h->vardata[i].loc = movpack[i].dest;
  }

  (void)f;

  free(movpack);
}

void gen_bracebody(struct checkstate *cs, struct objfile *f,
                   struct frame *h, struct ast_bracebody *a) {
  size_t vars_pushed = 0;

  for (size_t i = 0, e = a->statements_count; i < e; i++) {
    struct ast_statement *s = &a->statements[i];
    switch (s->tag) {
    case AST_STATEMENT_EXPR: {
      struct loc discard;
      gen_expr(cs, f, h, s->u.expr, &discard);
    } break;
    case AST_STATEMENT_RETURN_EXPR: {
      struct loc loc;
      gen_expr(cs, f, h, s->u.return_expr, &loc);

      gen_return(cs, f, h, loc);
    } break;
    case AST_STATEMENT_VAR: {
      struct loc loc;
      gen_expr(cs, f, h, s->u.var_statement.rhs, &loc);
      struct vardata vd;
      size_t var_number = h->var_number;
      h->var_number++;
      vardata_init(&vd, s->u.var_statement.decl.name.value,
                   var_number,
                   &s->u.var_statement.info.concrete_type,
                   kira_sizeof(&cs->nt, &s->u.var_statement.info.concrete_type),
                   loc);
      SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);
    } break;
    case AST_STATEMENT_GOTO: {
      struct labeldata *data = frame_try_find_labeldata(h, s->u.goto_statement.target.value);
      if (data) {
        /* The label is seen -- it has info on its var locations. */
        mov_and_add_vardata(data, h, f);
        /* TODO: Generate a jmp. */
      } else {
        /* The label is not seen yet -- plainly impose our var locations upon it! */
        struct labeldata ld;
        labeldata_init(&ld, s->u.goto_statement.target.value,
                       h->vardata, h->vardata_count,
                       0);
        SLICE_PUSH(h->labeldata, h->labeldata_count, h->labeldata_limit, ld);
        /* TODO: Generate a placeholder jmp. */
      }
    } break;
    case AST_STATEMENT_LABEL: {
      struct labeldata *data = frame_try_find_labeldata(h, s->u.label_statement.label.value);
      if (data) {
        /* The goto was seen -- it has info on our var locations. */
        CHECK(!data->label_found);
        mov_and_add_vardata(data, h, f);
        data->label_found = 1;
      } else {
        /* The goto was not seen yet -- define our own var locations! */
        struct labeldata ld;
        labeldata_init(&ld, s->u.label_statement.label.value,
                       h->vardata, h->vardata_count,
                       1);
        SLICE_PUSH(h->labeldata, h->labeldata_count, h->labeldata_limit, ld);
      }
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

  for (size_t i = 0; i < vars_pushed; i++) {
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  }
}

void gen_function_intro(struct objfile *f, struct frame *h) {
  (void)f, (void)h;
  /* TODO: push ebp, mov esp ebp */
}

void gen_function_exit(struct objfile *f, struct frame *h) {
  (void)f, (void)h;
  /* TODO: leave, ret */
}

void gen_lambda_expr(struct checkstate *cs, struct objfile *f,
                     struct ast_expr *a) {
  CHECK(a->tag == AST_EXPR_LAMBDA);
  struct frame h;
  frame_init(&h);

  note_param_locations(cs, &h, a);

  gen_function_intro(f, &h);

  gen_bracebody(cs, f, &h, &a->u.lambda.bracebody);

  gen_function_exit(f, &h);
  /* TODO: Someplace we have to update goto destinations and such. */

  frame_destroy(&h);
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
    objfile_fillercode_align_double_quadword(f);
    objfile_set_symbol_Value(f, inst->symbol_table_index,
                            objfile_section_size(objfile_text(f)));

    gen_lambda_expr(cs, f, &inst->value.u.typechecked_lambda);
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
