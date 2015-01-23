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

struct vardata {
  ident_value name;
  /* A non-owned reference to the type. */
  struct ast_typeexpr *concrete_type;
  /* The size of the type. */
  uint32_t size;
  struct loc loc;
};

void vardata_init(struct vardata *vd,
                  ident_value name,
                  struct ast_typeexpr *concrete_type,
                  uint32_t size,
                  struct loc initial_loc) {
  vd->name = name;
  vd->concrete_type = concrete_type;
  vd->size = size;
  vd->loc = initial_loc;
}

void vardata_destroy(struct vardata *vd) {
  vd->name = IDENT_VALUE_INVALID;
  vd->concrete_type = NULL;
  vd->size = 0;
  vd->loc.tag = (enum loc_tag)-1;
}

struct frame {
  struct vardata *vardata;
  size_t vardata_count;
  size_t vardata_limit;
};

void frame_init(struct frame *h) {
  h->vardata = NULL;
  h->vardata_count = 0;
  h->vardata_limit = 0;
}

void frame_destroy(struct frame *h) {
  SLICE_FREE(h->vardata, h->vardata_count, vardata_destroy);
  h->vardata_limit = 0;
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

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 param_type, size, loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    offset = int32_add(offset,
                       uint32_to_int32(uint32_ceil_aligned(size, DWORD_SIZE)));
  }
}

void gen_lambda_expr(struct checkstate *cs, struct objfile *f,
                     struct ast_expr *expr) {
  CHECK(expr->tag == AST_EXPR_LAMBDA);
  struct frame h;
  frame_init(&h);

  note_param_locations(cs, &h, expr);

  (void)f; /* TODO */

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
