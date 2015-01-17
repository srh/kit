#include "build.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "checkstate.h"
#include "databuf.h"
#include "io.h"
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

/* X86 */
enum x86_datareg {
  X86_DATAREG_EAX,
  X86_DATAREG_ECX,
  X86_DATAREG_EDX,
  X86_DATAREG_EBX,
  X86_DATAREG_ESI,
  X86_DATAREG_EDI,
};

enum locinfo_tag {
  /* The memory address ebp + u.ebp_offset */
  LOCINFO_EBP_OFFSET,
  /* The memory address stored in (ebp + u.ebp_offset) */
  LOCINFO_DEREF_EBP_OFFSET,
  /* X86 */
  /* The given register (for <= 4-byte values). */
  LOCINFO_DATAREG,
  /* The given two registers (for <= 8-byte values). */
  LOCINFO_TWO_DATAREGS,
};

struct two_dataregs {
  enum x86_datareg lo;
  enum x86_datareg hi;
};

struct locinfo {
  enum locinfo_tag tag;
  union {
    uint32_t ebp_offset;
    uint32_t deref_ebp_offset;
    enum x86_datareg datareg;
    struct two_dataregs two_dataregs;
  } u;
};

struct locinfo ebp_offset(uint32_t offset) {
  struct locinfo ret;
  ret.tag = LOCINFO_EBP_OFFSET;
  ret.u.ebp_offset = offset;
  return ret;
}

struct locinfo deref_ebp_offset(uint32_t offset) {
  struct locinfo ret;
  ret.tag = LOCINFO_DEREF_EBP_OFFSET;
  ret.u.deref_ebp_offset = offset;
  return ret;
}

struct locinfo datareg(enum x86_datareg reg) {
  struct locinfo ret;
  ret.tag = LOCINFO_DATAREG;
  ret.u.datareg = reg;
  return ret;
}

struct locinfo two_dataregs(enum x86_datareg lo, enum x86_datareg hi) {
  struct locinfo ret;
  ret.tag = LOCINFO_TWO_DATAREGS;
  ret.u.two_dataregs.lo = lo;
  ret.u.two_dataregs.hi = hi;
  return ret;
}

struct varinfo {
  ident_value name;
  struct ast_typeexpr type;
  struct locinfo loc;
};

void varinfo_init(struct varinfo *vi, ident_value name,
                  struct ast_typeexpr *type, struct locinfo loc) {
  vi->name = name;
  ast_typeexpr_init_copy(&vi->type, type);
  vi->loc = loc;
}

void varinfo_destroy(struct varinfo *vi) {
  vi->name = IDENT_VALUE_INVALID;
  ast_typeexpr_destroy(&vi->type);
}

struct varstate {
  struct varinfo *infos;
  size_t infos_count;
  size_t infos_limit;
};

void varstate_init(struct varstate *vs) {
  vs->infos = NULL;
  vs->infos_count = 0;
  vs->infos_limit = 0;
}

void varstate_destroy(struct varstate *vs) {
  CHECK(vs->infos_count == 0);
  free(vs->infos);
}

void varstate_push_var(struct varstate *vs, ident_value name,
                       struct ast_typeexpr *type, struct locinfo loc) {
  struct varinfo vi;
  varinfo_init(&vi, name, type, loc);
  SLICE_PUSH(vs->infos, vs->infos_count, vs->infos_limit, vi);
}

void varstate_pop_var(struct varstate *vs) {
  SLICE_POP(vs->infos, vs->infos_count, varinfo_destroy);
}

struct gen_kiracall {
  size_t num_vars;
  struct locinfo return_location;
};

void emit_push_ebp(struct objfile *f) {
  /* X86 */
  objfile_section_append_raw(objfile_text(f), "\x55\x8B\xEC", 3);
}

void emit_leave_ret(struct objfile *f) {
  /* X86 */
  objfile_section_append_raw(objfile_text(f), "\x5D\xC3", 2);
}

#define DWORD_SIZE 4

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

void gen_kiracall_start(struct checkstate *cs,
                        struct objfile *f,
                        struct varstate *vs,
                        struct gen_kiracall *kc,
                        struct ast_lambda *lambda,
                        struct ast_typeexpr *concrete_type) {
  CHECK(typeexpr_is_func_type(cs->im, concrete_type));
  CHECK(concrete_type->tag == AST_TYPEEXPR_APP);
  CHECK(concrete_type->u.app.params_count
        == size_add(lambda->params_count, 1));

  emit_push_ebp(f);

  /* X86 (and WINDOWS?) */
  uint32_t var_ebp_offset = 8;
  size_t ret_param = concrete_type->u.app.params_count - 1;

  struct locinfo return_location;
  uint32_t return_sizeof
    = kira_sizeof(&cs->nt, &concrete_type->u.app.params[ret_param]);
  if (return_sizeof > 8) {
    return_location = deref_ebp_offset(var_ebp_offset);
    var_ebp_offset = uint32_add(var_ebp_offset, DWORD_SIZE);
  } else if (return_sizeof > 4) {
    /* X86 + WINDOWS: return value (if it's not a raw double) is spread
       in eax/edx */
    return_location = two_dataregs(X86_DATAREG_EAX, X86_DATAREG_EDX);
  } else {
    return_location = datareg(X86_DATAREG_EAX);
  }

  for (size_t i = 0, e = ret_param; i < e; i++) {
    struct ast_typeexpr *param_type = &concrete_type->u.app.params[i];
    varstate_push_var(vs, lambda->params[i].name.value,
                      param_type, ebp_offset(var_ebp_offset));
    var_ebp_offset = uint32_add(var_ebp_offset,
                                kira_sizeof(&cs->nt, param_type));
  }

  kc->num_vars = lambda->params_count;
  kc->return_location = return_location;
}

void gen_kiracall_destroy(struct varstate *vs, struct gen_kiracall *kc) {
  for (size_t i = 0, e = kc->num_vars; i < e; i++) {
    varstate_pop_var(vs);
  }

  kc->num_vars = 0;
}

void gen_kiracall_finish(struct objfile *f, struct varstate *vs,
                         struct gen_kiracall *kc) {
  emit_leave_ret(f);
  gen_kiracall_destroy(vs, kc);
}

int build_bracebody(struct checkstate *cs, struct objfile *f,
                    struct varstate *vs, struct locinfo return_location,
                    struct ast_bracebody *x) {
  (void)cs, (void)f, (void)vs, (void)return_location, (void)x;
  /* TODO: Implement. */
  ERR_DBG("build_bracebody: not implemented.\n");
  return 0;
}

int build_lambda_instantiation(struct checkstate *cs, struct objfile *f,
                               struct def_instantiation *inst,
                               struct ast_expr *x) {
  CHECK(x->tag == AST_EXPR_LAMBDA);
  struct ast_lambda *lambda = &x->u.lambda;

  objfile_fillercode_align_double_quadword(f);

  objfile_set_symbol_Value(f, inst->symbol_table_index,
                           objfile_section_size(objfile_text(f)));

  struct varstate vs;
  varstate_init(&vs);

  struct gen_kiracall kc;
  gen_kiracall_start(cs, f, &vs, &kc, lambda, &inst->type);

  int ret = 0;
  if (!build_bracebody(cs, f, &vs, kc.return_location, &lambda->bracebody)) {
    goto cleanup_kiracall;
  }

  gen_kiracall_finish(f, &vs, &kc);

  ret = 1;
  goto cleanup_varstate;
 cleanup_kiracall:
  gen_kiracall_destroy(&vs, &kc);
 cleanup_varstate:
  varstate_destroy(&vs);
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
    return build_lambda_instantiation(cs, f, inst,
                                      &inst->value.u.typechecked_lambda);
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
