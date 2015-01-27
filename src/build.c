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
#include "x86.h"

struct loc;

void gen_mov(struct objfile *f, struct loc dest, struct loc src);

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

enum immediate_tag {
  IMMEDIATE_FUNC,
  IMMEDIATE_U32,
  IMMEDIATE_I32,
};

struct immediate {
  enum immediate_tag tag;
  union {
    uint32_t func_sti;
    uint32_t u32;
    int32_t i32;
  } u;
};

int immediate_equal(struct immediate a, struct immediate b) {
  if (a.tag != b.tag) {
    return 0;
  }
  switch (a.tag) {
  case IMMEDIATE_FUNC:
    return a.u.func_sti == b.u.func_sti;
  case IMMEDIATE_U32:
    return a.u.u32 == b.u.u32;
  case IMMEDIATE_I32:
    return a.u.i32 == b.u.i32;
  default:
    UNREACHABLE();
  }
}

enum loc_tag {
  LOC_EBP_OFFSET = 0,
  LOC_GLOBAL = 1,
  LOC_IMMEDIATE = 2,
  LOC_REGISTER = 3,
  LOC_INDIRECT = 8,
  LOC_EBP_INDIRECT = 8,
};

struct loc {
  enum loc_tag tag;
  uint32_t size;
  union {
    int32_t ebp_offset;
    uint32_t global_sti;
    struct immediate imm;
    int reg;
  } u;
};

struct loc ebp_loc(uint32_t size, int32_t ebp_offset) {
  struct loc ret;
  ret.tag = LOC_EBP_OFFSET;
  ret.size = size;
  ret.u.ebp_offset = ebp_offset;
  return ret;
}

struct loc global_loc(uint32_t size, uint32_t global_sti) {
  struct loc ret;
  ret.tag = LOC_GLOBAL;
  ret.size = size;
  ret.u.global_sti = global_sti;
  return ret;
}

struct loc ebp_indirect_loc(uint32_t size, int32_t ebp_offset) {
  struct loc ret;
  ret.tag = LOC_EBP_INDIRECT;
  ret.size = size;
  ret.u.ebp_offset = ebp_offset;
  return ret;
}

enum x86_reg {
  X86_EAX,
  X86_ECX,
  X86_EDX,
  X86_EBX,
  X86_ESP,
  X86_EBP,
  X86_ESI,
  X86_EDI,
};

/* TODO: Does anybody use this? */
int loc_equal(struct loc a, struct loc b) {
  if (a.tag != b.tag || a.size != b.size) {
    return 0;
  }
  switch (a.tag & (LOC_INDIRECT - 1)) {
  case LOC_EBP_OFFSET:
    return a.u.ebp_offset == b.u.ebp_offset;
  case LOC_GLOBAL:
    return a.u.global_sti == b.u.global_sti;
  case LOC_IMMEDIATE:
    return immediate_equal(a.u.imm, b.u.imm);
  case LOC_REGISTER:
    return a.u.reg == b.u.reg;
  default:
    UNREACHABLE();
  }
}

int loc_in_memory(struct loc a) {
  return a.tag != LOC_REGISTER && a.tag != LOC_IMMEDIATE;
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
  size_t target_number;

  int label_found;
};

void labeldata_init(struct labeldata *ld, ident_value labelname,
                    size_t target_number,
                    int label_found) {
  ld->labelname = labelname;
  ld->target_number = target_number;

  ld->label_found = label_found;
}

void labeldata_destroy(struct labeldata *ld) {
  ld->labelname = IDENT_VALUE_INVALID;
  ld->label_found = 0;
}

struct targetdata {
  int target_known;
  size_t target_offset;
};

struct jmpdata {
  size_t target_number;
  size_t jmp_location;
};

struct reset_esp_data {
  /* The .text offset where we need to reset esp. */
  size_t reset_esp_offset;
  /* esp's current offset relative to ebp, when we need to reset it.
     (This is a nonpositive value). */
  int32_t ebp_offset;
  /* Says whether we're setting esp to ebp_offset or setting esp back
     from ebp_offset. */
  int downward;
};

struct frame {
  size_t var_number;

  struct vardata *vardata;
  size_t vardata_count;
  size_t vardata_limit;

  struct labeldata *labeldata;
  size_t labeldata_count;
  size_t labeldata_limit;

  struct targetdata *targetdata;
  size_t targetdata_count;
  size_t targetdata_limit;

  struct jmpdata *jmpdata;
  size_t jmpdata_count;
  size_t jmpdata_limit;

  struct reset_esp_data *espdata;
  size_t espdata_count;
  size_t espdata_limit;

  int return_loc_valid;
  struct loc return_loc;

  int return_target_valid;
  size_t return_target_number;

  /* The offset (relative to ebp) of "free space" available --
     e.g. esp is at or below this address, but you could wipe anywhere
     below here without harm. */
  int32_t stack_offset;
  /* min_stack_offset tells us where to put esp.  TODO: Must esp be 8-
     or 16-byte aligned?  For function calls?  Remember that ret ptr
     and ebp take up 8 bytes.  */
  int32_t min_stack_offset;
};

void frame_init(struct frame *h) {
  h->var_number = 0;

  h->vardata = NULL;
  h->vardata_count = 0;
  h->vardata_limit = 0;

  h->labeldata = NULL;
  h->labeldata_count = 0;
  h->labeldata_limit = 0;

  h->targetdata = NULL;
  h->targetdata_count = 0;
  h->targetdata_limit = 0;

  h->jmpdata = NULL;
  h->jmpdata_count = 0;
  h->jmpdata_limit = 0;

  h->espdata = NULL;
  h->espdata_count = 0;
  h->espdata_limit = 0;

  h->return_loc_valid = 0;

  h->return_target_valid = 0;

  h->stack_offset = 0;
  h->min_stack_offset = 0;
}

void frame_destroy(struct frame *h) {
  SLICE_FREE(h->vardata, h->vardata_count, vardata_destroy);
  h->vardata_limit = 0;
  SLICE_FREE(h->labeldata, h->labeldata_count, labeldata_destroy);
  h->labeldata_limit = 0;
  free(h->targetdata);
  h->targetdata = NULL;
  h->targetdata_count = 0;
  h->targetdata_limit = 0;
  free(h->jmpdata);
  h->jmpdata = NULL;
  h->jmpdata_count = 0;
  h->jmpdata_limit = 0;
  free(h->espdata);
  h->espdata = NULL;
  h->espdata_count = 0;
  h->espdata_limit = 0;
}

void frame_specify_return_loc(struct frame *h, struct loc loc) {
  /* TODO: No need to specify the return loc -- just specify its size. */
  CHECK(!h->return_loc_valid);
  h->return_loc_valid = 1;
  h->return_loc = loc;
}

struct loc frame_return_loc(struct frame *h) {
  CHECK(h->return_loc_valid);
  return h->return_loc;
}

size_t frame_add_target(struct frame *h) {
  size_t ret = h->targetdata_count;
  struct targetdata td;
  td.target_known = 0;
  SLICE_PUSH(h->targetdata, h->targetdata_count, h->targetdata_limit, td);
  return ret;
}

void frame_define_target(struct frame *h, size_t target_number, uint32_t target_offset) {
  struct targetdata *td = &h->targetdata[target_number];
  CHECK(!td->target_known);
  td->target_known = 1;
  td->target_offset = target_offset;
}

struct loc register_loc(struct frame *h) {
  (void)h;
  /* TODO: Implement for real?  This is broken?  Or is eax always okay? */
  struct loc ret;
  ret.tag = LOC_REGISTER;
  ret.size = DWORD_SIZE;
  ret.u.reg = X86_EAX;
  return ret;
}

struct loc frame_push_loc(struct frame *h, uint32_t size) {
  /* X86 */
  uint32_t aligned = uint32_ceil_aligned(size, DWORD_SIZE);
  h->stack_offset = int32_sub(h->stack_offset, uint32_to_int32(aligned));
  if (h->stack_offset < h->min_stack_offset) {
    h->min_stack_offset = h->stack_offset;
  }
  return ebp_loc(size, h->stack_offset);
}

int32_t frame_save_offset(struct frame *h) {
  return h->stack_offset;
}

void frame_restore_offset(struct frame *h, int32_t stack_offset) {
  /* The stack is made _smaller_ by this function by increasing the
     offset (which is nonpositive). */
  CHECK(stack_offset <= 0);
  CHECK(stack_offset >= h->stack_offset);
  h->stack_offset = stack_offset;
}

void frame_pop(struct frame *h, uint32_t size) {
  /* X86 */
  uint32_t aligned = uint32_ceil_aligned(size, DWORD_SIZE);
  h->stack_offset = int32_add(h->stack_offset, uint32_to_int32(aligned));
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

  int32_t offset = (2 + (return_type_size > 2 * DWORD_SIZE)) * DWORD_SIZE;

  for (size_t i = 0, e = expr->u.lambda.params_count; i < e; i++) {
    struct ast_typeexpr *param_type = &type->u.app.params[i];

    uint32_t size = kira_sizeof(&cs->nt, param_type);

    struct loc loc = ebp_loc(size, offset);

    size_t var_number = h->var_number;
    h->var_number++;

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 var_number, param_type, size, loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    offset = int32_add(offset,
                       uint32_to_int32(uint32_ceil_aligned(size, DWORD_SIZE)));
  }

  if (return_type_size > 2 * DWORD_SIZE) {
    struct loc loc = ebp_indirect_loc(return_type_size, 2 * DWORD_SIZE);
    frame_specify_return_loc(h, loc);
  } else if (return_type_size > DWORD_SIZE) {
    /* The return location is eax:edx, we shouldn't be specifying
       return_loc like this. */
    TODO_IMPLEMENT;
  } else {
    struct loc loc;
    loc.tag = LOC_REGISTER;
    loc.size = return_type_size;
    loc.u.reg = X86_EAX;
    frame_specify_return_loc(h, loc);
  }
}

int lookup_vardata_by_name(struct frame *h, ident_value name, size_t *index_out) {
  for (size_t i = 0, e = h->vardata_count; i < e; i++) {
    if (h->vardata[i].name == name) {
      *index_out = i;
      return 1;
    }
  }
  return 0;
}

int gen_immediate_numeric_literal(struct ast_numeric_literal *a,
                                  struct loc *return_val_out) {
  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED: {
    int32_t value;
    if (!numeric_literal_to_i32(a->digits, a->digits_count, &value)) {
      return 0;
    }

    struct loc loc;
    loc.tag = LOC_IMMEDIATE;
    loc.size = DWORD_SIZE;
    loc.u.imm.tag = IMMEDIATE_I32;
    loc.u.imm.u.i32 = value;
    *return_val_out = loc;
    return 1;
  } break;
  case AST_NUMERIC_TYPE_UNSIGNED: {
    uint32_t value;
    if (!numeric_literal_to_u32(a->digits, a->digits_count, &value)) {
      return 0;
    }

    struct loc loc;
    loc.tag = LOC_IMMEDIATE;
    loc.size = DWORD_SIZE;
    loc.u.imm.tag = IMMEDIATE_U32;
    loc.u.imm.u.u32 = value;
    *return_val_out = loc;
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

uint8_t mod_reg_rm(int mod, int reg, int rm) {
  return (uint8_t)((mod << 6) | (reg << 3) | rm);
}

#define MOD00 0
#define MOD01 1
#define MOD10 2
#define MOD11 3

void x86_gen_push32(struct objfile *f, enum x86_reg reg) {
  uint8_t b = 0x50 + (uint8_t)reg;
  objfile_section_append_raw(objfile_text(f), &b, 1);
}

void x86_gen_pop32(struct objfile *f, enum x86_reg reg) {
  uint8_t b = 0x58 + (uint8_t)reg;
  objfile_section_append_raw(objfile_text(f), &b, 1);
}

void x86_gen_ret(struct objfile *f) {
  uint8_t b = 0xC3;
  objfile_section_append_raw(objfile_text(f), &b, 1);
}

void x86_gen_mov_reg32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x8B;
  b[1] = mod_reg_rm(MOD11, dest, src);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_mov_reg_imm32(struct objfile *f, enum x86_reg dest,
                           struct immediate imm) {
  uint8_t b = 0xB8 + (uint8_t)dest;
  objfile_section_append_raw(objfile_text(f), &b, 1);
  switch (imm.tag) {
  case IMMEDIATE_FUNC: {
    /* TODO: Is dir32 the right one? */
    objfile_section_append_dir32(objfile_text(f), imm.u.func_sti);
  } break;
  case IMMEDIATE_U32: {
    /* LITTLEENDIAN etc. */
    objfile_section_append_raw(objfile_text(f), &imm.u.u32, sizeof(uint32_t));
  } break;
  case IMMEDIATE_I32: {
    /* LITTLEENDIAN etc. */
    objfile_section_append_raw(objfile_text(f), &imm.u.i32, sizeof(int32_t));
  } break;
  default:
    UNREACHABLE();
  }
}

void x86_gen_add_esp_i32(struct objfile *f, int32_t x) {
  uint8_t b[6];
  b[0] = 0x81;
  b[1] = mod_reg_rm(MOD11, 0, X86_ESP);
  STATIC_CHECK(sizeof(int32_t) == 4);
  ok_memcpy(b + 2, &x, sizeof(int32_t));
  objfile_section_append_raw(objfile_text(f), b, 6);
}

void x86_gen_add_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x01;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_sub_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x29;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void gen_placeholder_stack_adjustment(struct objfile *f,
                                      struct frame *h,
                                      int downward) {
  struct reset_esp_data red;
  red.reset_esp_offset = objfile_section_size(objfile_text(f));
  red.ebp_offset = h->stack_offset;
  red.downward = downward;
  SLICE_PUSH(h->espdata, h->espdata_count, h->espdata_limit, red);
  uint8_t b[6] = { 0x81, 0 };
  b[1] = mod_reg_rm(MOD11, 0, X86_ESP);
  objfile_section_append_raw(objfile_text(f), b, 6);
}

void replace_placeholder_stack_adjustment(struct objfile *f,
                                          size_t location,
                                          int32_t stack_adjustment) {
  objfile_section_overwrite_raw(objfile_text(f),
                                location + 2,
                                &stack_adjustment,
                                sizeof(stack_adjustment));
}

/* Check callers if max count returned increases. */
size_t x86_encode_reg_rm(uint8_t *b, int reg, enum x86_reg rm_addr,
                         int32_t rm_addr_disp) {
  if (rm_addr_disp == 0 && rm_addr != X86_ESP && rm_addr != X86_EBP) {
    b[0] = mod_reg_rm(MOD00, reg, rm_addr);
    return 1;
  } else if (rm_addr == X86_ESP) {
    TODO_IMPLEMENT;
  } else if (rm_addr_disp >= -128 && rm_addr_disp <= 127) {
    b[0] = mod_reg_rm(MOD01, reg, rm_addr);
    b[1] = (int8_t)rm_addr_disp;
    return 2;
  } else {
    b[0] = mod_reg_rm(MOD10, reg, rm_addr);
    STATIC_CHECK(sizeof(rm_addr_disp) == 4);
    ok_memcpy(b + 1, &rm_addr_disp, sizeof(rm_addr_disp));
    return 5;
  }
}

void x86_gen_load32(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                    int32_t src_disp) {
  uint8_t b[10];
  b[0] = 0x8B;
  size_t count = x86_encode_reg_rm(b + 1, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

int x86_reg_has_lowbyte(enum x86_reg reg) {
  return reg == X86_EAX || reg == X86_ECX || reg == X86_EDX || reg == X86_EBX;
}

void x86_gen_load8(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                   int32_t src_disp) {
  CHECK(x86_reg_has_lowbyte(dest));
  uint8_t b[10];
  b[0] = 0x8A;
  size_t count = x86_encode_reg_rm(b + 1, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

void x86_gen_store32(struct objfile *f, enum x86_reg dest_addr, uint32_t dest_disp,
                     enum x86_reg src) {
  uint8_t b[10];
  b[0] = 0x89;
  size_t count = x86_encode_reg_rm(b + 1, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

void x86_gen_store8(struct objfile *f, enum x86_reg dest_addr, uint32_t dest_disp,
                    enum x86_reg src) {
  CHECK(x86_reg_has_lowbyte(src));
  uint8_t b[10];
  b[0] = 0x88;
  size_t count = x86_encode_reg_rm(b + 1, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

void gen_function_intro(struct objfile *f, struct frame *h) {
  /* X86 */
  x86_gen_push32(f, X86_EBP);
  x86_gen_mov_reg32(f, X86_EBP, X86_ESP);
  gen_placeholder_stack_adjustment(f, h, 1);
}

void gen_function_exit(struct objfile *f, struct frame *h) {
  if (h->return_target_valid) {
    frame_define_target(h, h->return_target_number,
                        objfile_section_size(objfile_text(f)));
  }
  x86_gen_mov_reg32(f, X86_ESP, X86_EBP);
  x86_gen_pop32(f, X86_EBP);
  x86_gen_ret(f);
}

enum x86_reg choose_register_2(enum x86_reg used1, enum x86_reg used2) {
  if (used1 == X86_EAX || used2 == X86_EAX) {
    if (used1 == X86_ECX || used2 == X86_ECX) {
      return X86_EDX;
    } else {
      return X86_ECX;
    }
  } else {
    return X86_EAX;
  }
}

/* Chooses a register (one of EAX, ECX, EDX) that doesn't conflict
   with register_user. */
enum x86_reg choose_register(struct loc register_user) {
  int tag_lo = (register_user.tag & (LOC_INDIRECT - 1));
  if (tag_lo == LOC_REGISTER) {
    if (register_user.u.reg == X86_EAX) {
      return X86_ECX;
    } else {
      return X86_EAX;
    }
  } else {
    return X86_EAX;
  }
}

struct loc move_to_register(struct objfile *f, struct loc a, struct loc user) {
  CHECK(a.size <= DWORD_SIZE);
  /* First move a to a register. */
  if (a.tag == LOC_REGISTER) {
    return a;
  } else {
    struct loc regloc;
    regloc.tag = LOC_REGISTER;
    regloc.size = a.size;
    regloc.u.reg = choose_register(user);
    gen_mov(f, regloc, a);
    return regloc;
  }
}

void move_to_registers(struct objfile *f, struct loc a, struct loc b,
                       struct loc *a_out, struct loc *b_out) {
  CHECK(a.size <= DWORD_SIZE);
  CHECK(b.size <= DWORD_SIZE);

  a = move_to_register(f, a, b);
  b = move_to_register(f, b, a);
  *a_out = a;
  *b_out = b;
}

void gen_mov_addressof(struct objfile *f, struct loc dest, struct loc memory_loc) {
  (void)f, (void)dest, (void)memory_loc;
  TODO_IMPLEMENT;
}

/* Makes the memory not be "too" indirect. */
struct loc make_movable(struct objfile *f, struct loc loc, struct loc register_user) {
  if (loc.tag & LOC_INDIRECT) {
    int tag_lo = (loc.tag & (LOC_INDIRECT - 1));
    if (tag_lo == LOC_EBP_OFFSET) {
      struct loc x;
      x.tag = LOC_REGISTER;
      x.size = DWORD_SIZE;
      x.u.reg = choose_register(register_user);

      gen_mov_addressof(f, x, loc);

      x.size = loc.size;
      x.tag |= LOC_INDIRECT;
      return x;
    } else if (tag_lo == LOC_REGISTER) {
      return loc;
    } else {
      CRASH("Bad indirect loc.\n");
    }
  } else {
    return loc;
  }
}

void gen_memmem_mov(struct objfile *f,
                    enum x86_reg dest_reg,
                    int32_t dest_disp,
                    enum x86_reg src_reg,
                    int32_t src_disp,
                    uint32_t usize) {
  int32_t size = uint32_to_int32(usize);
  enum x86_reg reg = choose_register_2(dest_reg, src_reg);
  int32_t n = 0;
  while (n < size) {
    if (size - n >= DWORD_SIZE) {
      x86_gen_load32(f, reg, src_reg, int32_add(n, src_disp));
      x86_gen_store32(f, dest_reg, int32_add(n, dest_disp), reg);
      n += DWORD_SIZE;
    } else {
      x86_gen_load8(f, reg, src_reg, int32_add(n, src_disp));
      x86_gen_store8(f, dest_reg, int32_add(n, dest_disp), reg);
      n += 1;
    }
  }
}

/* When we call this, the only things using registers (besides esp and
   ebp) are dest or loc. */
void gen_mov(struct objfile *f, struct loc dest, struct loc src) {
  CHECK(dest.size == src.size);
  if (loc_equal(dest, src)) {
    return;
  }

  CHECK((dest.tag & (LOC_INDIRECT - 1)) != LOC_IMMEDIATE);
  CHECK((dest.tag & (LOC_INDIRECT - 1)) != LOC_GLOBAL);

  dest = make_movable(f, dest, src);
  src = make_movable(f, src, dest);

  if (dest.tag == (LOC_REGISTER | LOC_INDIRECT)) {
    if (src.tag == (LOC_REGISTER | LOC_INDIRECT)) {
      gen_memmem_mov(f, dest.u.reg, 0, src.u.reg, 0, src.size);
    } else {
      TODO_IMPLEMENT;
    }
  } else if (dest.tag == LOC_EBP_OFFSET) {
    if (src.tag == (LOC_REGISTER | LOC_INDIRECT)) {
      gen_memmem_mov(f, X86_EBP, dest.u.ebp_offset, src.u.reg, 0, src.size);
    } else if (src.tag == LOC_EBP_OFFSET) {
      gen_memmem_mov(f, X86_EBP, dest.u.ebp_offset, X86_EBP, src.u.ebp_offset, src.size);
    } else if (src.tag == LOC_REGISTER) {
      if (src.size == DWORD_SIZE) {
        x86_gen_store32(f, X86_EBP, dest.u.ebp_offset, src.u.reg);
      } else {
        TODO_IMPLEMENT;
      }
    } else {
      TODO_IMPLEMENT;
    }
  } else if (dest.tag == LOC_REGISTER) {
    CHECK(dest.size <= DWORD_SIZE);
    if (src.tag == (LOC_REGISTER | LOC_INDIRECT)) {
      CHECK(dest.size == DWORD_SIZE);
      x86_gen_load32(f, dest.u.reg, src.u.reg, 0);
    } else if (src.tag == LOC_EBP_OFFSET) {
      CHECK(dest.size == DWORD_SIZE);
      x86_gen_load32(f, dest.u.reg, X86_EBP, src.u.ebp_offset);
    } else if (src.tag == LOC_REGISTER) {
      CHECK(dest.size == DWORD_SIZE);
      x86_gen_mov_reg32(f, dest.u.reg, src.u.reg);
    } else if (src.tag == LOC_GLOBAL) {
      TODO_IMPLEMENT;
    } else if (src.tag == LOC_IMMEDIATE) {
      x86_gen_mov_reg_imm32(f, dest.u.reg, src.u.imm);
    }
  } else {
    UNREACHABLE();
  }
}

void x86_gen_call(struct objfile *f, uint32_t func_sti) {
  uint8_t b = 0xE8;
  objfile_section_append_raw(objfile_text(f), &b, 1);
  objfile_section_append_rel32(objfile_text(f), func_sti);
}

/* An expr_return tells how gen_expr should provide the return value. */
enum expr_return_tag {
  /* The location where the data is to be returned is precisely
     specified. */
  EXPR_RETURN_DEMANDED,
  /* The location is not specified -- gen_expr should write the
     data's location to "loc", and preserve lvalues. */
  EXPR_RETURN_OPEN,
  /* The expr returns a boolean -- and instead of returning, it should
     jmp to the given target, if true. */
  EXPR_RETURN_JMP_IF_TRUE,
  /* Likewise, only negated. */
  EXPR_RETURN_JMP_IF_FALSE,
};

struct expr_return {
  enum expr_return_tag tag;
  union {
    struct loc loc;
    size_t jmp_target_number;
  } u;
};

void expr_return_set(struct objfile *f, struct expr_return *er, struct loc loc) {
  switch (er->tag) {
  case EXPR_RETURN_DEMANDED: {
    gen_mov(f, er->u.loc, loc);
  } break;
  case EXPR_RETURN_OPEN: {
    er->u.loc = loc;
  } break;
  case EXPR_RETURN_JMP_IF_TRUE: {
    /* This should be a boolean. */
    CHECK(loc.size == 1);
    TODO_IMPLEMENT;
  } break;
  case EXPR_RETURN_JMP_IF_FALSE: {
    /* This should be a boolean. */
    CHECK(loc.size == 1);
    TODO_IMPLEMENT;
  } break;
  default:
    UNREACHABLE();
  }
}

struct expr_return open_expr_return(void) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_OPEN;
  return ret;
}

struct expr_return demand_expr_return(struct loc loc) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_DEMANDED;
  ret.u.loc = loc;
  return ret;
}

int gen_expr(struct checkstate *cs, struct objfile *f,
             struct frame *h, struct ast_expr *a,
             struct expr_return *ret);

int gen_funcall_expr(struct checkstate *cs, struct objfile *f,
                     struct frame *h, struct ast_expr *a,
                     struct expr_return *er) {
  size_t args_count = a->u.funcall.args_count;

  uint32_t return_size = kira_sizeof(&cs->nt,
                                     &a->expr_info.concrete_type);

  /* X86 */
  int memory_demanded = (er->tag == EXPR_RETURN_DEMANDED
                         && (er->u.loc.tag == LOC_EBP_OFFSET
                             || er->u.loc.tag == LOC_GLOBAL
                             || (er->u.loc.tag & LOC_INDIRECT)));

  struct loc return_loc;
  if (memory_demanded) {
    return_loc = er->u.loc;
  } else {
    return_loc = frame_push_loc(h, return_size);
  }

  for (size_t i = args_count; i > 0;) {
    i--;

    struct ast_expr *arg = &a->u.funcall.args[i];

    struct loc arg_loc = frame_push_loc(h, kira_sizeof(&cs->nt,
                                                       &arg->expr_info.concrete_type));

    struct expr_return er = demand_expr_return(arg_loc);
    int32_t saved_offset = frame_save_offset(h);
    if (!gen_expr(cs, f, h, arg, &er)) {
      return 0;
    }
    frame_restore_offset(h, saved_offset);
  }

  if (return_size > 2 * DWORD_SIZE) {
    struct loc ptr_loc = frame_push_loc(h, DWORD_SIZE);
    gen_mov_addressof(f, ptr_loc, return_loc);
  }

  struct expr_return func_er = open_expr_return();
  int32_t saved_offset = frame_save_offset(h);
  if (!gen_expr(cs, f, h, a->u.funcall.func, &func_er)) {
    return 0;
  }

  if (func_er.u.loc.tag == LOC_IMMEDIATE) {
    frame_restore_offset(h, saved_offset);
    CHECK(func_er.u.loc.u.imm.tag == IMMEDIATE_FUNC);
    gen_placeholder_stack_adjustment(f, h, 0);
    x86_gen_call(f, func_er.u.loc.u.imm.u.func_sti);
    gen_placeholder_stack_adjustment(f, h, 1);
  } else {
    /* TODO: frame_restore_offset after loading func into register. */
    ERR_DBG("Indirect function calls not yet supported.\n");
    return 0;
    /* TODO: Support indirect function calls. */
  }

  if (return_size <= DWORD_SIZE) {
    /* Return value in eax. */
    struct loc eax_loc;
    eax_loc.tag = LOC_REGISTER;
    eax_loc.size = return_size;
    eax_loc.u.reg = X86_EAX;
    gen_mov(f, return_loc, eax_loc);
  } else if (return_size <= 2 * DWORD_SIZE) {
    /* We need to copy eax:edx into return_loc. */
    TODO_IMPLEMENT;
  } else {
    /* Value already in return_loc. */
  }

  expr_return_set(f, er, return_loc);
  return 1;
}

void gen_i32_negate(struct objfile *f, struct frame *h,
                    struct loc dest, struct loc src) {
  (void)f, (void)h, (void)dest, (void)src;
  TODO_IMPLEMENT;
}

int gen_unop_expr(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_expr *a,
                  struct expr_return *er) {
  struct ast_unop_expr *ue = &a->u.unop_expr;
  switch (ue->operator) {
  case AST_UNOP_DEREFERENCE: {
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, ue->rhs, &rhs_er)) {
      return 0;
    }

    if (rhs_er.u.loc.tag & LOC_INDIRECT) {
      struct loc ret = register_loc(h);
      gen_mov(f, ret, rhs_er.u.loc);
      ret.size = kira_sizeof(&cs->nt, &a->expr_info.concrete_type);
      ret.tag |= LOC_INDIRECT;
      expr_return_set(f, er, ret);
      return 1;
    } else {
      struct loc ret = rhs_er.u.loc;
      ret.size = kira_sizeof(&cs->nt, &a->expr_info.concrete_type);
      ret.tag |= LOC_INDIRECT;
      expr_return_set(f, er, ret);
      return 1;
    }
  } break;
  case AST_UNOP_ADDRESSOF: {
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, ue->rhs, &rhs_er)) {
      return 0;
    }

    struct loc ret = register_loc(h);
    gen_mov_addressof(f, ret, rhs_er.u.loc);
    expr_return_set(f, er, ret);
    return 1;
  } break;
  case AST_UNOP_NEGATE: {
    /* Right now the only type of negation is i32 negation. */
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, ue->rhs, &rhs_er)) {
      return 0;
    }

    struct loc ret = register_loc(h);
    gen_i32_negate(f, h, ret, rhs_er.u.loc);
    expr_return_set(f, er, ret);
    /* TODO: gen_negate needs to jump upon overflow. */
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}


void gen_placeholder_jmp(struct objfile *f, struct frame *h, size_t target_number) {
  struct jmpdata jd;
  jd.target_number = target_number;
  jd.jmp_location = objfile_section_size(objfile_text(f));
  SLICE_PUSH(h->jmpdata, h->jmpdata_count, h->jmpdata_limit, jd);
  /* X86 */
  uint8_t b[5] = { 0xE9, 0, 0, 0, 0 };
  objfile_section_append_raw(objfile_text(f), b, 5);
}

void replace_placeholder_jmp(struct objfile *f, size_t jmp_location,
                             size_t target_offset) {
  int32_t target32 = size_to_int32(target_offset);
  int32_t jmp32 = size_to_int32(size_add(jmp_location, 5));
  int32_t diff = int32_sub(target32, jmp32);
  objfile_section_overwrite_raw(objfile_text(f),
                                jmp_location + 1,
                                &diff,
                                sizeof(diff));
}

struct loc memory_loc_make_use_no_registers(struct objfile *f, struct frame *h,
                                            struct loc loc) {
  CHECK(loc_in_memory(loc));
  switch (loc.tag & (LOC_INDIRECT - 1)) {
  case LOC_EBP_OFFSET:  /* fall-through */
  case LOC_GLOBAL:  /* fall-through */
  case LOC_IMMEDIATE:
    return loc;
  case LOC_REGISTER: {
    CHECK(loc.tag & LOC_INDIRECT);
    loc.tag = LOC_REGISTER;
    uint32_t size = loc.size;
    loc.size = DWORD_SIZE;
    struct loc newloc = frame_push_loc(h, DWORD_SIZE);
    gen_mov(f, newloc, loc);
    CHECK(!(newloc.tag & LOC_INDIRECT));
    newloc.tag |= LOC_INDIRECT;
    newloc.size = size;
    return newloc;
  } break;
  default:
    UNREACHABLE();
  }
}

struct loc gen_loc_without_registers(struct objfile *f, struct frame *h,
                                     struct loc loc) {
  (void)f, (void)h;  /* TODO */
  switch (loc.tag & (LOC_INDIRECT - 1)) {
  case LOC_EBP_OFFSET:  /* fall-through */
  case LOC_GLOBAL:  /* fall-through */
  case LOC_IMMEDIATE:
    return loc;
  case LOC_REGISTER: {
    TODO_IMPLEMENT;
  } break;
  default:
    UNREACHABLE();
  }
}

enum numeric_type get_numeric_type(struct identmap *im, struct ast_typeexpr *a) {
  CHECK(a->tag == AST_TYPEEXPR_NAME);
  const void *buf;
  size_t count;
  identmap_lookup(im, a->u.name.value, &buf, &count);
  if (count == strlen(BYTE_TYPE_NAME) && 0 == memcmp(buf, BYTE_TYPE_NAME, count)) {
    return NUMERIC_TYPE_BYTE;
  }
  if (count == strlen(I32_TYPE_NAME) && 0 == memcmp(buf, I32_TYPE_NAME, count)) {
    return NUMERIC_TYPE_I32;
  }
  if (count == strlen(U32_TYPE_NAME) && 0 == memcmp(buf, U32_TYPE_NAME, count)) {
    return NUMERIC_TYPE_U32;
  }
  CRASH("Expected a numeric type.");
}

int gen_binop_expr(struct checkstate *cs, struct objfile *f,
                   struct frame *h, struct ast_expr *a,
                   struct expr_return *er) {
  struct ast_binop_expr *be = &a->u.binop_expr;
  switch (be->operator) {
  case AST_BINOP_ASSIGN: {
    struct expr_return lhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, be->lhs, &lhs_er)) {
      return 0;
    }
    /* lhs_er is a location of an lvalue.  We want to make it not use
       any registers (so that we can dumbly evaluate the rhs) and
       assign the rhs to it. */
    lhs_er.u.loc = memory_loc_make_use_no_registers(f, h, lhs_er.u.loc);
    lhs_er.tag = EXPR_RETURN_DEMANDED;
    if (!gen_expr(cs, f, h, be->rhs, &lhs_er)) {
      return 0;
    }

    /* gen_expr above did our assignment.  Our expression has a return
       value though, expr_return_set does that. */
    expr_return_set(f, er, lhs_er.u.loc);
    return 1;
  } break;
  case AST_BINOP_LOGICAL_OR:
    TODO_IMPLEMENT;
  case AST_BINOP_LOGICAL_AND:
    TODO_IMPLEMENT;
  default: {
    enum numeric_type lhs_type = get_numeric_type(cs->im, &be->lhs->expr_info.concrete_type);
    enum numeric_type rhs_type = get_numeric_type(cs->im, &be->rhs->expr_info.concrete_type);
    CHECK(lhs_type == rhs_type);

    struct expr_return lhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, be->lhs, &lhs_er)) {
      return 0;
    }
    lhs_er.u.loc = gen_loc_without_registers(f, h, lhs_er.u.loc);
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, be->rhs, &rhs_er)) {
      return 0;
    }

    size_t size = numeric_type_size(lhs_type);
    CHECK(lhs_er.u.loc.size == size);
    CHECK(rhs_er.u.loc.size == size);

    struct loc lhs_reg;
    struct loc rhs_reg;
    move_to_registers(f, lhs_er.u.loc, rhs_er.u.loc,
                      &lhs_reg, &rhs_reg);

    switch (be->operator) {
    case AST_BINOP_ADD: {
      switch (lhs_type) {
      case NUMERIC_TYPE_BYTE: {
        TODO_IMPLEMENT;
      } break;
      case NUMERIC_TYPE_I32: {
        x86_gen_add_w32(f, lhs_reg.u.reg, rhs_reg.u.reg);
        /* TODO: Handle overflow. */
      } break;
      case NUMERIC_TYPE_U32: {
        x86_gen_add_w32(f, lhs_reg.u.reg, rhs_reg.u.reg);
        /* TODO: Handle overflow. */
      } break;
      default:
        UNREACHABLE();
      }
    } break;
    case AST_BINOP_SUB: {
      switch (lhs_type) {
      case NUMERIC_TYPE_BYTE: {
        TODO_IMPLEMENT;
      } break;
      case NUMERIC_TYPE_I32: {
        x86_gen_sub_w32(f, lhs_reg.u.reg, rhs_reg.u.reg);
        /* TODO: Handle overflow. */
      } break;
      case NUMERIC_TYPE_U32: {
        x86_gen_sub_w32(f, lhs_reg.u.reg, rhs_reg.u.reg);
        /* TODO: Handle overflow. */
      } break;
      }
    } break;
    default:
      TODO_IMPLEMENT;
    }

    expr_return_set(f, er, lhs_reg);
    return 1;
  } break;
  }
}

int gen_expr(struct checkstate *cs, struct objfile *f,
             struct frame *h, struct ast_expr *a,
             struct expr_return *er) {
  switch (a->tag) {
  case AST_EXPR_NAME: {
    CHECK(a->u.name.info.info_valid);
    struct def_instantiation *inst = a->u.name.info.inst_or_null;
    if (inst) {
      CHECK(inst->symbol_table_index_computed);
      if (typeexpr_is_func_type(cs->im, &inst->type)) {
        struct loc loc;
        loc.tag = LOC_IMMEDIATE;
        loc.size = DWORD_SIZE;
        loc.u.imm.tag = IMMEDIATE_FUNC;
        loc.u.imm.u.func_sti = inst->symbol_table_index;
        expr_return_set(f, er, loc);
      } else {
        struct loc loc = global_loc(kira_sizeof(&cs->nt, &inst->type),
                                    inst->symbol_table_index);
        expr_return_set(f, er, loc);
      }
    } else {
      size_t vi;
      int found_vi = lookup_vardata_by_name(h, a->u.name.ident.value, &vi);
      CHECK(found_vi);
      expr_return_set(f, er, h->vardata[vi].loc);
    }
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    struct loc loc;
    if (!gen_immediate_numeric_literal(&a->u.numeric_literal, &loc)) {
      return 0;
    }
    expr_return_set(f, er, loc);
    return 1;
  } break;
  case AST_EXPR_FUNCALL: {
    return gen_funcall_expr(cs, f, h, a, er);
  } break;
  case AST_EXPR_UNOP: {
    return gen_unop_expr(cs, f, h, a, er);
  } break;
  case AST_EXPR_BINOP: {
    return gen_binop_expr(cs, f, h, a, er);
  } break;
  default:
    TODO_IMPLEMENT;
  }
}

void gen_return(struct objfile *f, struct frame *h) {
  if (!h->return_target_valid) {
    h->return_target_valid = 1;
    h->return_target_number = frame_add_target(h);
  }
  gen_placeholder_jmp(f, h, h->return_target_number);
}

int gen_bracebody(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_bracebody *a) {
  size_t vars_pushed = 0;
  int32_t initial_stack_offset = h->stack_offset;

  for (size_t i = 0, e = a->statements_count; i < e; i++) {
    struct ast_statement *s = &a->statements[i];
    switch (s->tag) {
    case AST_STATEMENT_EXPR: {
      int32_t saved_offset = frame_save_offset(h);
      struct expr_return er = open_expr_return();
      if (!gen_expr(cs, f, h, s->u.expr, &er)) {
        return 0;
      }
      frame_restore_offset(h, saved_offset);
    } break;
    case AST_STATEMENT_RETURN_EXPR: {
      int32_t saved_offset = frame_save_offset(h);
      struct expr_return er = demand_expr_return(frame_return_loc(h));
      if (!gen_expr(cs, f, h, s->u.return_expr, &er)) {
        return 0;
      }

      gen_return(f, h);
      frame_restore_offset(h, saved_offset);
    } break;
    case AST_STATEMENT_VAR: {
      uint32_t var_size = kira_sizeof(&cs->nt, &s->u.var_statement.info.concrete_type);
      struct loc var_loc = frame_push_loc(h, var_size);

      int32_t saved_offset = frame_save_offset(h);

      struct expr_return er = demand_expr_return(var_loc);
      if (!gen_expr(cs, f, h, s->u.var_statement.rhs, &er)) {
        return 0;
      }
      frame_restore_offset(h, saved_offset);

      struct vardata vd;
      size_t var_number = h->var_number;
      h->var_number++;
      vardata_init(&vd, s->u.var_statement.decl.name.value,
                   var_number,
                   &s->u.var_statement.info.concrete_type,
                   var_size,
                   var_loc);
      SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);
      vars_pushed++;
    } break;
    case AST_STATEMENT_GOTO: {
      struct labeldata *data = frame_try_find_labeldata(h, s->u.goto_statement.target.value);
      if (data) {
        /* The label is seen -- it has info on its var locations. */
        gen_placeholder_jmp(f, h, data->target_number);
      } else {
        size_t target_number = frame_add_target(h);
        /* The label is not seen yet -- plainly impose our var locations upon it! */
        struct labeldata ld;
        labeldata_init(&ld, s->u.goto_statement.target.value,
                       target_number,
                       0);
        SLICE_PUSH(h->labeldata, h->labeldata_count, h->labeldata_limit, ld);
        gen_placeholder_jmp(f, h, target_number);
      }
    } break;
    case AST_STATEMENT_LABEL: {
      struct labeldata *data = frame_try_find_labeldata(h, s->u.label_statement.label.value);
      if (data) {
        /* The goto was seen -- it has info on our var locations. */
        CHECK(!data->label_found);
        data->label_found = 1;
        frame_define_target(h, data->target_number, objfile_section_size(objfile_text(f)));
      } else {
        size_t target_number = frame_add_target(h);
        frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

        /* The goto was not seen yet -- define our own var locations! */
        struct labeldata ld;
        labeldata_init(&ld, s->u.label_statement.label.value,
                       target_number,
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
    /* TODO: Just do frame_reset or something, it's more reliable. */
    frame_pop(h, h->vardata[size_sub(h->vardata_count, 1)].size);
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  }

  CHECK(h->stack_offset == initial_stack_offset);
  return 1;
}

void tie_gotos(struct objfile *f, struct frame *h) {
  for (size_t i = 0, e = h->jmpdata_count; i < e; i++) {
    struct jmpdata jd = h->jmpdata[i];
    CHECK(jd.target_number < h->targetdata_count);
    struct targetdata td = h->targetdata[jd.target_number];
    CHECK(td.target_known);
    replace_placeholder_jmp(f, jd.jmp_location, td.target_offset);
  }
}

void tie_stack_adjustments(struct objfile *f, struct frame *h) {
  for (size_t i = 0, e = h->espdata_count; i < e; i++) {
    struct reset_esp_data red = h->espdata[i];
    replace_placeholder_stack_adjustment(
        f, red.reset_esp_offset,
        red.downward
        ? int32_sub(h->min_stack_offset, red.ebp_offset)
        : int32_sub(red.ebp_offset, h->min_stack_offset));
  }
}

int gen_lambda_expr(struct checkstate *cs, struct objfile *f,
                    struct ast_expr *a) {
  CHECK(a->tag == AST_EXPR_LAMBDA);
  struct frame h;
  frame_init(&h);

  note_param_locations(cs, &h, a);

  gen_function_intro(f, &h);

  int res = gen_bracebody(cs, f, &h, &a->u.lambda.bracebody);

  if (res) {
    gen_function_exit(f, &h);
    tie_gotos(f, &h);
    tie_stack_adjustments(f, &h);
  }

  frame_destroy(&h);
  return res;
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
