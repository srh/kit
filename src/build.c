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
struct frame;

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

enum x86_reg8 {
  X86_AL,
  X86_CL,
  X86_DL,
  X86_BL,
  X86_AH,
  X86_CH,
  X86_DH,
  X86_BH,
};

/* Only applicable for 0F-prefixed off32 instructions (I think). */
enum x86_setcc {
  X86_SETCC_L = 0x9C,
  X86_SETCC_LE = 0x9E,
  X86_SETCC_G = 0x9F,
  X86_SETCC_GE = 0x9D,
  X86_SETCC_E = 0x94,
  X86_SETCC_NE = 0x95,
  X86_SETCC_A = 0x97,
  X86_SETCC_AE = 0x93,
  X86_SETCC_B = 0x92,
  X86_SETCC_BE = 0x96,
};

/* Only applicable for 0F-prefixed jmp instructions (I think). */
enum x86_jcc {
  X86_JCC_O = 0x80,
  X86_JCC_Z = 0x84,
  X86_JCC_S = 0x88,
  X86_JCC_A = 0x87,
  X86_JCC_C = 0x82,
};

/* Returns true if the reg has a lobyte (also, the register code
   happens to identify the lowbyte in a reg or modr/m field). */
int x86_reg_has_lowbyte(enum x86_reg reg) {
  return reg == X86_EAX || reg == X86_ECX || reg == X86_EDX || reg == X86_EBX;
}

void gen_mov(struct objfile *f, struct loc dest, struct loc src);
void gen_store_register(struct objfile *f, struct loc dest, enum x86_reg reg);
void gen_crash_jcc(struct objfile *f, struct frame *h, enum x86_jcc code);

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
  IMMEDIATE_PRIMITIVE_OP,
  IMMEDIATE_U32,
  IMMEDIATE_I32,
};

struct immediate {
  enum immediate_tag tag;
  union {
    uint32_t func_sti;
    enum primitive_op primitive_op;
    uint32_t u32;
    int32_t i32;
  } u;
};

enum loc_tag {
  LOC_EBP_OFFSET,
  LOC_GLOBAL,
  LOC_EBP_INDIRECT,
};

struct loc {
  enum loc_tag tag;
  uint32_t size;
  uint32_t padded_size;
  union {
    int32_t ebp_offset;
    uint32_t global_sti;
    int32_t ebp_indirect;
  } u;
};

struct loc ebp_loc(uint32_t size, uint32_t padded_size, int32_t ebp_offset) {
  CHECK(size <= padded_size && padded_size - size < DWORD_SIZE);
  struct loc ret;
  ret.tag = LOC_EBP_OFFSET;
  ret.size = size;
  ret.padded_size = padded_size;
  ret.u.ebp_offset = ebp_offset;
  return ret;
}

struct loc global_loc(uint32_t size, uint32_t padded_size, uint32_t global_sti) {
  CHECK(size <= padded_size && padded_size - size < DWORD_SIZE);
  struct loc ret;
  ret.tag = LOC_GLOBAL;
  ret.size = size;
  ret.padded_size = padded_size;
  ret.u.global_sti = global_sti;
  return ret;
}

struct loc ebp_indirect_loc(uint32_t size, uint32_t padded_size, int32_t ebp_offset) {
  CHECK(size <= padded_size && padded_size - size < DWORD_SIZE);
  struct loc ret;
  ret.tag = LOC_EBP_INDIRECT;
  ret.size = size;
  ret.padded_size = padded_size;
  ret.u.ebp_indirect = ebp_offset;
  return ret;
}

int loc_equal(struct loc a, struct loc b) {
  if (a.tag != b.tag || a.size != b.size) {
    return 0;
  }
  switch (a.tag) {
  case LOC_EBP_OFFSET:
    return a.u.ebp_offset == b.u.ebp_offset;
  case LOC_GLOBAL:
    return a.u.global_sti == b.u.global_sti;
  case LOC_EBP_INDIRECT:
    return a.u.ebp_indirect == b.u.ebp_indirect;
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

  int crash_target_exists;
  size_t crash_target_number;

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

  h->crash_target_exists = 0;

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

struct loc frame_push_loc(struct frame *h, uint32_t size) {
  /* X86 */
  uint32_t padded_size = uint32_ceil_aligned(size, DWORD_SIZE);
  h->stack_offset = int32_sub(h->stack_offset, uint32_to_int32(padded_size));
  if (h->stack_offset < h->min_stack_offset) {
    h->min_stack_offset = h->stack_offset;
  }
  return ebp_loc(size, padded_size, h->stack_offset);
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

int exists_hidden_return_param(uint32_t return_type_size) {
  return !(return_type_size <= 2 || return_type_size == DWORD_SIZE
           || return_type_size == 2 * DWORD_SIZE);
}

/* X86 and maybe WINDOWS-specific calling convention stuff. */
void note_param_locations(struct checkstate *cs, struct frame *h, struct ast_expr *expr) {
  struct ast_typeexpr *type = ast_expr_type(expr);
  size_t args_count = expr->u.lambda.params_count;
  struct ast_typeexpr *return_type
    = expose_func_return_type(cs->im, type, size_add(args_count, 1));

  uint32_t return_type_size = kira_sizeof(&cs->nt, return_type);

  int32_t offset = (2 + exists_hidden_return_param(return_type_size)) * DWORD_SIZE;

  for (size_t i = 0, e = expr->u.lambda.params_count; i < e; i++) {
    struct ast_typeexpr *param_type = &type->u.app.params[i];

    uint32_t size = kira_sizeof(&cs->nt, param_type);
    uint32_t padded_size = uint32_ceil_aligned(size, DWORD_SIZE);

    struct loc loc = ebp_loc(size, padded_size, offset);

    size_t var_number = h->var_number;
    h->var_number++;

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 var_number, param_type, size, loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    offset = int32_add(offset, uint32_to_int32(padded_size));
  }

  if (exists_hidden_return_param(return_type_size)) {
    /* I don't know yet if WINDOWS promises padding, so we assume none. */
    struct loc loc = ebp_indirect_loc(return_type_size,
                                      return_type_size,
                                      2 * DWORD_SIZE);
    frame_specify_return_loc(h, loc);
  } else {
    struct loc loc = frame_push_loc(h, return_type_size);
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

void x86_gen_mov_reg8(struct objfile *f, enum x86_reg8 dest, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0x8A;
  b[1] = mod_reg_rm(MOD11, dest, src);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_test_regs32(struct objfile *f, enum x86_reg reg1, enum x86_reg reg2) {
  uint8_t b[2];
  b[0] = 0x85;
  b[1] = mod_reg_rm(MOD11, reg2, reg1);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

/* Appends funcaddrs with dir32 -- not suitable for relative address
   call instructions. */
void append_immediate(struct objfile *f, struct immediate imm) {
  switch (imm.tag) {
  case IMMEDIATE_FUNC: {
    objfile_section_append_dir32(objfile_text(f), imm.u.func_sti);
  } break;
  case IMMEDIATE_PRIMITIVE_OP: {
    CRASH("Trying to put a primitive op immediate in memory.\n");
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

void x86_gen_mov_reg_imm32(struct objfile *f, enum x86_reg dest,
                           struct immediate imm) {
  uint8_t b = 0xB8 + (uint8_t)dest;
  objfile_section_append_raw(objfile_text(f), &b, 1);
  append_immediate(f, imm);
}

void x86_gen_mov_reg_stiptr(struct objfile *f, enum x86_reg dest,
                            uint32_t symbol_table_index) {
  uint8_t b = 0xB8 + (uint8_t)dest;
  objfile_section_append_raw(objfile_text(f), &b, 1);
  objfile_section_append_dir32(objfile_text(f), symbol_table_index);
}

void x86_gen_int_3(struct objfile *f) {
  uint8_t b = 0xCC;
  objfile_section_append_raw(objfile_text(f), &b, 1);
}

void x86_gen_shl_cl_w32(struct objfile *f, enum x86_reg dest) {
  uint8_t b[2];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0xD3;
  b[1] = mod_reg_rm(MOD11, 4, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_shl_cl_w8(struct objfile *f, enum x86_reg8 dest) {
  uint8_t b[2];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0xD2;
  b[1] = mod_reg_rm(MOD11, 4, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_shr_cl_w32(struct objfile *f, enum x86_reg dest) {
  uint8_t b[2];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0xD3;
  b[1] = mod_reg_rm(MOD11, 5, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_shr_cl_w8(struct objfile *f, enum x86_reg8 dest) {
  uint8_t b[2];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0xD2;
  b[1] = mod_reg_rm(MOD11, 5, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_sar_cl_w32(struct objfile *f, enum x86_reg dest) {
  uint8_t b[2];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0xD3;
  b[1] = mod_reg_rm(MOD11, 7, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
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

void x86_gen_add_w8(struct objfile *f, enum x86_reg8 dest, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0x00;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_eaxedx_mul_w32(struct objfile *f, enum x86_reg src) {
  uint8_t b[2];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0xF7;
  b[1] = mod_reg_rm(MOD11, 4, src);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_alah_mul_w8(struct objfile *f, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 4, src);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_imul_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[3];
  b[0] = 0x0F;
  b[1] = 0xAF;
  b[2] = mod_reg_rm(MOD11, dest, src);
  objfile_section_append_raw(objfile_text(f), b, 3);
}

void x86_gen_eaxedx_div_w32(struct objfile *f, enum x86_reg denom) {
  uint8_t b[2];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0xF7;
  b[1] = mod_reg_rm(MOD11, 6, denom);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_alah_div_w8(struct objfile *f, enum x86_reg8 denom) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 6, denom);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_eaxedx_idiv_w32(struct objfile *f, enum x86_reg denom) {
  uint8_t b[2];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0xF7;
  b[1] = mod_reg_rm(MOD11, 7, denom);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_cdq_w32(struct objfile *f) {
  uint8_t b = 0x99;
  objfile_section_append_raw(objfile_text(f), &b, 1);
}

void x86_gen_cmp_w32(struct objfile *f, enum x86_reg lhs, enum x86_reg rhs) {
  uint8_t b[2];
  b[0] = 0x39;
  b[1] = mod_reg_rm(MOD11, rhs, lhs);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_cmp_imm32(struct objfile *f, enum x86_reg lhs, int32_t imm32) {
  uint8_t b[6];
  b[0] = 0x81;
  b[1] = mod_reg_rm(MOD11, 7, lhs);
  /* LITTLEENDIAN */
  CHECK(sizeof(imm32) == 4);
  memcpy(b + 2, &imm32, sizeof(imm32));
  objfile_section_append_raw(objfile_text(f), b, 6);
}

void x86_gen_xor_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x31;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_or_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x09;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_and_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x21;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_neg_w32(struct objfile *f, enum x86_reg dest) {
  uint8_t b[2];
  b[0] = 0xF7;
  b[1] = mod_reg_rm(MOD11, 3, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_sub_w32(struct objfile *f, enum x86_reg dest, enum x86_reg src) {
  uint8_t b[2];
  b[0] = 0x29;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_sub_w8(struct objfile *f, enum x86_reg8 dest, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0x28;
  b[1] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_setcc_b8(struct objfile *f, enum x86_reg8 dest, enum x86_setcc code) {
  uint8_t b[3];
  b[0] = 0x0F;
  b[1] = (uint8_t)code;
  b[2] = mod_reg_rm(MOD11, 0, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
}

void gen_placeholder_jcc(struct objfile *f, struct frame *h,
                         enum x86_jcc code, size_t target_number) {
  struct jmpdata jd;
  jd.target_number = target_number;
  /* X86 */
  jd.jmp_location = 2 + objfile_section_size(objfile_text(f));
  SLICE_PUSH(h->jmpdata, h->jmpdata_count, h->jmpdata_limit, jd);

  uint8_t b[6] = { 0x0F, 0, 0, 0, 0, 0 };
  b[1] = code;
  objfile_section_append_raw(objfile_text(f), b, 6);
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
    CRASH("Encoding esp here is not supported.");
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

void x86_gen_mov_mem_imm32(struct objfile *f,
                           enum x86_reg dest,
                           int32_t dest_disp,
                           struct immediate imm) {
  uint8_t b[10];
  b[0] = 0xC7;
  size_t count = x86_encode_reg_rm(b + 1, 0, dest, dest_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
  append_immediate(f, imm);
}

void x86_gen_load32(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                    int32_t src_disp) {
  uint8_t b[10];
  b[0] = 0x8B;
  size_t count = x86_encode_reg_rm(b + 1, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

void x86_gen_movzx8(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                    int32_t src_disp) {
  uint8_t b[11];
  b[0] = 0x0F;
  b[1] = 0xB6;
  size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 2);
}

void x86_gen_movzx8_reg8(struct objfile *f, enum x86_reg dest, enum x86_reg8 src) {
  uint8_t b[3];
  b[0] = 0x0F;
  b[1] = 0xB6;
  b[2] = mod_reg_rm(MOD11, dest, src);
  objfile_section_append_raw(objfile_text(f), b, 3);
}

void x86_gen_lea32(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                   int32_t src_disp) {
  uint8_t b[10];
  b[0] = 0x8D;
  size_t count = x86_encode_reg_rm(b + 1, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

void x86_gen_load8(struct objfile *f, enum x86_reg8 dest, enum x86_reg src_addr,
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
  } else {
    CRASH("return_target_valid false (no return statements?)");
  }

  if (exists_hidden_return_param(h->return_loc.size)) {
    CHECK(h->return_loc.tag == LOC_EBP_INDIRECT);
    CHECK(h->return_loc.u.ebp_indirect == 8);
    x86_gen_load32(f, X86_EAX, X86_EBP, h->return_loc.u.ebp_indirect);
  } else if (h->return_loc.size <= DWORD_SIZE) {
    CHECK(h->return_loc.tag == LOC_EBP_OFFSET);
    CHECK(h->return_loc.padded_size == DWORD_SIZE);
    x86_gen_load32(f, X86_EAX, X86_EBP, h->return_loc.u.ebp_offset);
  } else {
    CHECK(h->return_loc.size == 2 * DWORD_SIZE);
    CHECK(h->return_loc.tag == LOC_EBP_OFFSET);
    CHECK(h->return_loc.padded_size == 2 * DWORD_SIZE);
    x86_gen_load32(f, X86_EAX, X86_EBP, h->return_loc.u.ebp_offset);
    x86_gen_load32(f, X86_EDX, X86_EBP, int32_add(h->return_loc.u.ebp_offset, DWORD_SIZE));
  }

  x86_gen_mov_reg32(f, X86_ESP, X86_EBP);
  x86_gen_pop32(f, X86_EBP);
  x86_gen_ret(f);

  if (h->crash_target_exists) {
    frame_define_target(h, h->crash_target_number,
                        objfile_section_size(objfile_text(f)));
    /* TODO: I don't know what I really want to do here.  Call a
       Kira-specific function?  Call abort?  (What does abort do?) */
    x86_gen_int_3(f);
  }
}

enum x86_reg choose_altreg(enum x86_reg used) {
  if (used == X86_EAX) {
    return X86_ECX;
  } else {
    return X86_EAX;
  }
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

void gen_mov_addressof(struct objfile *f, struct loc dest, struct loc loc) {
  CHECK(dest.size == DWORD_SIZE);
  switch (loc.tag) {
  case LOC_EBP_OFFSET: {
    x86_gen_lea32(f, X86_EAX, X86_EBP, loc.u.ebp_offset);
    gen_store_register(f, dest, X86_EAX);
  } break;
  case LOC_GLOBAL: {
    x86_gen_mov_reg_stiptr(f, X86_EAX, loc.u.global_sti);
    gen_store_register(f, dest, X86_EAX);
  } break;
  case LOC_EBP_INDIRECT: {
    x86_gen_load32(f, X86_EAX, X86_EBP, loc.u.ebp_indirect);
    gen_store_register(f, dest, X86_EAX);
  } break;
  default:
    UNREACHABLE();
  }
}

void gen_memmem_mov(struct objfile *f,
                    enum x86_reg dest_reg,
                    int32_t dest_disp,
                    enum x86_reg src_reg,
                    int32_t src_disp,
                    uint32_t upadded_size) {
  int32_t padded_size = uint32_to_int32(upadded_size);
  enum x86_reg reg = choose_register_2(dest_reg, src_reg);
  int32_t n = 0;
  while (n < padded_size) {
    if (padded_size - n >= DWORD_SIZE) {
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

void put_ptr_in_reg(struct objfile *f, struct loc loc, enum x86_reg free_reg,
                    enum x86_reg *reg_out, int32_t *disp_out) {
  switch (loc.tag) {
  case LOC_EBP_OFFSET: {
    *reg_out = X86_EBP;
    *disp_out = loc.u.ebp_offset;
  } break;
  case LOC_GLOBAL: {
    x86_gen_mov_reg_stiptr(f, free_reg, loc.u.global_sti);
    *reg_out = free_reg;
    *disp_out = 0;
  } break;
  case LOC_EBP_INDIRECT: {
    x86_gen_load32(f, free_reg, X86_EBP, loc.u.ebp_indirect);
    *reg_out = free_reg;
    *disp_out = 0;
  } break;
  default:
    UNREACHABLE();
  }
}

/* When we call this, the only things using registers (besides esp and
   ebp) are dest or loc.  It's safe to use this if dest and src point
   to the same _exact_ memory location, through different means (or
   through the same means). */
void gen_mov(struct objfile *f, struct loc dest, struct loc src) {
  CHECK(dest.size == src.size);
  if (loc_equal(dest, src)) {
    return;
  }

  CHECK(dest.tag != LOC_GLOBAL);

  enum x86_reg dest_reg;
  int32_t dest_disp;
  put_ptr_in_reg(f, dest, X86_EAX, &dest_reg, &dest_disp);
  enum x86_reg src_reg;
  int32_t src_disp;
  put_ptr_in_reg(f, src, X86_EDX, &src_reg, &src_disp);

  uint32_t padded_size = dest.padded_size < src.padded_size ? dest.padded_size : src.padded_size;
  CHECK(padded_size >= src.size);
  gen_memmem_mov(f, dest_reg, dest_disp, src_reg, src_disp, padded_size);
}

void gen_store_register(struct objfile *f, struct loc dest, enum x86_reg reg) {
  CHECK(dest.size <= DWORD_SIZE);
  CHECK(dest.padded_size == DWORD_SIZE);
  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    x86_gen_store32(f, X86_EBP, dest.u.ebp_offset, reg);
    break;
  case LOC_GLOBAL:
    CRASH("Writing to globals is impossible.");
    break;
  case LOC_EBP_INDIRECT: {
    enum x86_reg altreg = choose_altreg(reg);
    x86_gen_load32(f, altreg, X86_EBP, dest.u.ebp_indirect);
    x86_gen_store32(f, altreg, 0, reg);
  } break;
  default:
    UNREACHABLE();
  }
}

void gen_load_register(struct objfile *f, enum x86_reg reg, struct loc src) {
  CHECK(src.size <= DWORD_SIZE);
  CHECK(src.padded_size == DWORD_SIZE);
  if (src.size == DWORD_SIZE) {
    switch (src.tag) {
    case LOC_EBP_OFFSET:
      x86_gen_load32(f, reg, X86_EBP, src.u.ebp_offset);
      break;
    case LOC_GLOBAL:
      x86_gen_mov_reg_stiptr(f, reg, src.u.global_sti);
      x86_gen_load32(f, reg, reg, 0);
      break;
    case LOC_EBP_INDIRECT:
      x86_gen_load32(f, reg, X86_EBP, src.u.ebp_indirect);
      x86_gen_load32(f, reg, reg, 0);
      break;
    default:
      UNREACHABLE();
    }
  } else if (src.size == 1) {
    switch (src.tag) {
    case LOC_EBP_OFFSET:
      x86_gen_movzx8(f, reg, X86_EBP, src.u.ebp_offset);
      break;
    case LOC_GLOBAL:
      x86_gen_mov_reg_stiptr(f, reg, src.u.global_sti);
      x86_gen_movzx8(f, reg, reg, 0);
      break;
    case LOC_EBP_INDIRECT:
      x86_gen_load32(f, reg, X86_EBP, src.u.ebp_indirect);
      x86_gen_movzx8(f, reg, reg, 0);
      break;
    default:
      UNREACHABLE();
    }
  } else {
    CRASH("not implemented or unreachable.");
  }
}



void gen_store_biregister(struct objfile *f, struct loc dest, enum x86_reg lo, enum x86_reg hi) {
  CHECK(DWORD_SIZE < dest.size && dest.size <= 2 * DWORD_SIZE);
  CHECK(dest.padded_size == 2 * DWORD_SIZE);
  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    x86_gen_store32(f, X86_EBP, dest.u.ebp_offset, lo);
    x86_gen_store32(f, X86_EBP, int32_add(dest.u.ebp_offset, DWORD_SIZE), hi);
    break;
  case LOC_GLOBAL: {
    CRASH("Writing to globals is impossible.");
  } break;
  case LOC_EBP_INDIRECT: {
    enum x86_reg altreg = choose_register_2(lo, hi);
    x86_gen_load32(f, altreg, X86_EBP, dest.u.ebp_indirect);
    x86_gen_store32(f, altreg, 0, lo);
    x86_gen_store32(f, altreg, DWORD_SIZE, hi);
  } break;
  default:
    UNREACHABLE();
  }
}

void gen_mov_immediate(struct objfile *f, struct loc dest, struct immediate src) {
  CHECK(dest.size == DWORD_SIZE);

  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    x86_gen_mov_mem_imm32(f, X86_EBP, dest.u.ebp_offset, src);
    break;
  case LOC_EBP_INDIRECT:
    x86_gen_load32(f, X86_EAX, X86_EBP, dest.u.ebp_indirect);
    x86_gen_mov_mem_imm32(f, X86_EAX, 0, src);
    break;
  case LOC_GLOBAL:
    CRASH("Global mutation should be impossible.");
  default:
    UNREACHABLE();
  }
}


void x86_gen_call(struct objfile *f, uint32_t func_sti) {
  uint8_t b = 0xE8;
  objfile_section_append_raw(objfile_text(f), &b, 1);
  objfile_section_append_rel32(objfile_text(f), func_sti);
}

void x86_gen_indirect_call_reg(struct objfile *f, enum x86_reg reg) {
  uint8_t b[2];
  b[0] = 0xFF;
  b[1] = mod_reg_rm(MOD11, 2, reg);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

/* An expr_return tells how gen_expr should provide the return value. */
enum expr_return_tag {
  /* The location where the data is to be returned is precisely
     specified. */
  EXPR_RETURN_DEMANDED,
  /* The location is not specified -- gen_expr should write the
     data's location to "loc", and preserve lvalues. */
  EXPR_RETURN_OPEN,
  EXPR_RETURN_FREE,
};

enum expr_return_free_tag {
  EXPR_RETURN_FREE_LOC,
  EXPR_RETURN_FREE_IMM,
};

struct expr_return_free {
  enum expr_return_free_tag tag;
  union {
    struct loc loc;
    struct immediate imm;
  } u;
};

struct expr_return {
  enum expr_return_tag tag;
  union {
    struct loc loc;
    size_t jmp_target_number;
    struct expr_return_free free;
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
  case EXPR_RETURN_FREE: {
    er->u.free.tag = EXPR_RETURN_FREE_LOC;
    er->u.free.u.loc = loc;
  } break;
  default:
    UNREACHABLE();
  }
}

struct expr_return free_expr_return(void) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_FREE;
  return ret;
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

void gen_cmp32_behavior(struct objfile *f,
                        int32_t off0, int32_t off1,
                        enum x86_setcc setcc_code) {
  x86_gen_load32(f, X86_EDX, X86_EBP, off0);
  x86_gen_load32(f, X86_ECX, X86_EBP, off1);
  x86_gen_cmp_w32(f, X86_EDX, X86_ECX);
  x86_gen_setcc_b8(f, X86_AL, setcc_code);
  x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
}

void gen_cmp8_behavior(struct objfile *f,
                       int32_t off0, int32_t off1,
                       enum x86_setcc setcc_code) {
  x86_gen_movzx8(f, X86_EDX, X86_EBP, off0);
  x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
  x86_gen_cmp_w32(f, X86_EDX, X86_ECX);
  x86_gen_setcc_b8(f, X86_EAX, setcc_code);
  x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
}

void gen_primitive_op_behavior(struct objfile *f,
                               struct frame *h,
                               enum primitive_op prim_op) {
  int32_t off0 = h->stack_offset;
  int32_t off1 = int32_add(h->stack_offset, DWORD_SIZE);
  switch (prim_op) {
  case PRIMITIVE_OP_CONVERT_BYTE_TO_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_BYTE_TO_I32: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_BYTE_TO_U32: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_BYTE: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs32(f, X86_EAX, X86_EAX);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_BYTE: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 255);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs32(f, X86_EAX, X86_EAX);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_NEGATE_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    /* Crashes if the value is INT32_MIN by subtracting 1 and
       overflowing. */
    x86_gen_cmp_imm32(f, X86_EAX, 1);
    gen_crash_jcc(f, h, X86_JCC_O);
    x86_gen_neg_w32(f, X86_EAX);
  } break;
  case PRIMITIVE_OP_ADD_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_SUB_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_MUL_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_imul_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_DIV_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_cdq_w32(f);
    x86_gen_eaxedx_idiv_w32(f, X86_ECX);
    /* Divide by zero or INT32_MIN / -1 will produce #DE. */
  } break;
  case PRIMITIVE_OP_MOD_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_cdq_w32(f);
    x86_gen_eaxedx_idiv_w32(f, X86_ECX);
    x86_gen_mov_reg32(f, X86_EAX, X86_EDX);
    /* Divide by zero or INT32_MIN / -1 will produce #DE. */
  } break;
  case PRIMITIVE_OP_LT_I32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_L);
  } break;
  case PRIMITIVE_OP_LE_I32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_LE);
  } break;
  case PRIMITIVE_OP_GT_I32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_G);
  } break;
  case PRIMITIVE_OP_GE_I32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_GE);
  } break;
  case PRIMITIVE_OP_EQ_I32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_I32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w32(f, X86_EAX);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_sar_cl_w32(f, X86_EAX);
  } break;

  case PRIMITIVE_OP_ADD_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_eaxedx_mul_w32(f, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_DIV_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_eaxedx_div_w32(f, X86_ECX);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_eaxedx_div_w32(f, X86_ECX);
    x86_gen_mov_reg32(f, X86_EAX, X86_EDX);
    /* Modulus by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_LT_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_B);
  } break;
  case PRIMITIVE_OP_LE_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_BE);
  } break;
  case PRIMITIVE_OP_GT_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_A);
  } break;
  case PRIMITIVE_OP_GE_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_AE);
  } break;
  case PRIMITIVE_OP_EQ_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w32(f, X86_EAX);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shr_cl_w32(f, X86_EAX);
  } break;

  case PRIMITIVE_OP_ADD_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w8(f, X86_AL, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w8(f, X86_AL, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_mul_w8(f, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_C);
    x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
  } break;
  case PRIMITIVE_OP_DIV_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_div_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
  } break;
  case PRIMITIVE_OP_MOD_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_div_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_mov_reg8(f, X86_AL, X86_AH);
  } break;
  case PRIMITIVE_OP_LT_BYTE: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_B);
  } break;
  case PRIMITIVE_OP_LE_BYTE: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_BE);
  } break;
  case PRIMITIVE_OP_GT_BYTE: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_A);
  } break;
  case PRIMITIVE_OP_GE_BYTE: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_AE);
  } break;
  case PRIMITIVE_OP_EQ_BYTE: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_BYTE: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w8(f, X86_AL);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_BYTE: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shr_cl_w8(f, X86_AL);
  } break;

  default:
    UNREACHABLE();
  }
}

int gen_funcall_expr(struct checkstate *cs, struct objfile *f,
                     struct frame *h, struct ast_expr *a,
                     struct expr_return *er) {
  size_t args_count = a->u.funcall.args_count;

  uint32_t return_size = kira_sizeof(&cs->nt, ast_expr_type(a));

  /* X86 */
  struct loc return_loc = frame_push_loc(h, return_size);

  for (size_t i = args_count; i > 0;) {
    i--;

    struct ast_expr *arg = &a->u.funcall.args[i];

    struct loc arg_loc = frame_push_loc(h, kira_sizeof(&cs->nt, ast_expr_type(arg)));

    struct expr_return er = demand_expr_return(arg_loc);
    int32_t saved_offset = frame_save_offset(h);
    if (!gen_expr(cs, f, h, arg, &er)) {
      return 0;
    }
    frame_restore_offset(h, saved_offset);
  }

  if (exists_hidden_return_param(return_size)) {
    struct loc ptr_loc = frame_push_loc(h, DWORD_SIZE);
    gen_mov_addressof(f, ptr_loc, return_loc);
  }

  int32_t saved_offset = frame_save_offset(h);
  struct expr_return func_er = free_expr_return();
  if (!gen_expr(cs, f, h, a->u.funcall.func, &func_er)) {
    return 0;
  }

  switch (func_er.u.free.tag) {
  case EXPR_RETURN_FREE_IMM: {
    frame_restore_offset(h, saved_offset);
    switch (func_er.u.free.u.imm.tag) {
    case IMMEDIATE_FUNC:
      gen_placeholder_stack_adjustment(f, h, 0);
      x86_gen_call(f, func_er.u.free.u.imm.u.func_sti);
      gen_placeholder_stack_adjustment(f, h, 1);
      break;
    case IMMEDIATE_PRIMITIVE_OP: {
      gen_primitive_op_behavior(f, h, func_er.u.free.u.imm.u.primitive_op);
    } break;
    case IMMEDIATE_U32:
    case IMMEDIATE_I32:
    default:
      UNREACHABLE();
    }
  } break;
  case EXPR_RETURN_FREE_LOC: {
    gen_load_register(f, X86_EAX, func_er.u.free.u.loc);
    frame_restore_offset(h, saved_offset);
    gen_placeholder_stack_adjustment(f, h, 0);
    x86_gen_indirect_call_reg(f, X86_EAX);
    gen_placeholder_stack_adjustment(f, h, 1);
  } break;
  }

  if (exists_hidden_return_param(return_size)) {
    /* We're going to act like the pointer returned by the callee (in
       EAX) could be pointing to something different than
       return_loc. */

    struct loc return_ptr_loc = frame_push_loc(h, DWORD_SIZE);
    gen_store_register(f, return_ptr_loc, X86_EAX);
    struct loc eax_return_val_loc = ebp_indirect_loc(return_size, return_size,
                                                     return_ptr_loc.u.ebp_offset);

    expr_return_set(f, er, eax_return_val_loc);
  } else if (return_size <= DWORD_SIZE) {
    /* Return value in eax. */
    gen_store_register(f, return_loc, X86_EAX);
    expr_return_set(f, er, return_loc);
  } else {
    CHECK(return_size == 2 * DWORD_SIZE);
    gen_store_biregister(f, return_loc, X86_EAX, X86_EDX);
    expr_return_set(f, er, return_loc);
  }

  return 1;
}

void apply_dereference(struct objfile *f,
                       struct frame *h, struct loc ptr_loc,
                       uint32_t pointee_size, struct expr_return *er) {
  struct loc loc = frame_push_loc(h, DWORD_SIZE);
  gen_mov(f, loc, ptr_loc);
  CHECK(loc.tag == LOC_EBP_OFFSET);

  /* When dereferencing a pointer, we have no info about padding, so
     the padded size is the same as the size. */
  struct loc ret = ebp_indirect_loc(pointee_size, pointee_size,
                                    loc.u.ebp_offset);
  expr_return_set(f, er, ret);
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

    uint32_t size = kira_sizeof(&cs->nt, ast_expr_type(a));
    apply_dereference(f, h, rhs_er.u.loc, size, er);
    return 1;
  } break;
  case AST_UNOP_ADDRESSOF: {
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, ue->rhs, &rhs_er)) {
      return 0;
    }

    struct loc ret = frame_push_loc(h, DWORD_SIZE);
    gen_mov_addressof(f, ret, rhs_er.u.loc);
    expr_return_set(f, er, ret);
    return 1;
  } break;
  case AST_UNOP_NEGATE:
  default:
    UNREACHABLE();
  }
}

void gen_placeholder_jmp_if_false(struct objfile *f, struct frame *h,
                                  struct loc loc, size_t target_number) {
  CHECK(loc.size == KIRA_BOOL_SIZE);

  gen_load_register(f, X86_EAX, loc);

  x86_gen_test_regs32(f, X86_EAX, X86_EAX);

  gen_placeholder_jcc(f, h, X86_JCC_Z, target_number);
}

void gen_crash_jcc(struct objfile *f, struct frame *h, enum x86_jcc code) {
  /* The crash target code gets generated (at the end) only if we call
     this function -- only if h->crash_target_exists is true. */
  if (!h->crash_target_exists) {
    h->crash_target_number = frame_add_target(h);
    h->crash_target_exists = 1;
  }

  gen_placeholder_jcc(f, h, code, h->crash_target_number);
}

void gen_placeholder_jmp(struct objfile *f, struct frame *h, size_t target_number) {
  struct jmpdata jd;
  jd.target_number = target_number;
  jd.jmp_location = 1 + objfile_section_size(objfile_text(f));
  SLICE_PUSH(h->jmpdata, h->jmpdata_count, h->jmpdata_limit, jd);
  /* X86 */
  /* E9 jmp instruction */
  uint8_t b[5] = { 0xE9, 0, 0, 0, 0 };
  objfile_section_append_raw(objfile_text(f), b, 5);
}

void replace_placeholder_jump(struct objfile *f, size_t jmp_location,
                              size_t target_offset) {
  int32_t target32 = size_to_int32(target_offset);
  int32_t jmp32 = size_to_int32(size_add(jmp_location, 4));
  int32_t diff = int32_sub(target32, jmp32);
  objfile_section_overwrite_raw(objfile_text(f),
                                jmp_location,
                                &diff,
                                sizeof(diff));
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

    lhs_er.tag = EXPR_RETURN_DEMANDED;
    if (!gen_expr(cs, f, h, be->rhs, &lhs_er)) {
      return 0;
    }

    /* gen_expr above did our assignment.  Our expression has a return
       value though, expr_return_set does that. */
    expr_return_set(f, er, lhs_er.u.loc);
    return 1;
  } break;
  case AST_BINOP_LOGICAL_OR: {
    struct expr_return lhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, be->lhs, &lhs_er)) {
      return 0;
    }

    struct loc ret = frame_push_loc(h, KIRA_BOOL_SIZE);

    size_t target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, lhs_er.u.loc, target_number);
    gen_mov(f, ret, lhs_er.u.loc);
    size_t end_target_number = frame_add_target(h);

    gen_placeholder_jmp(f, h, end_target_number);
    frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

    struct expr_return rhs_er = demand_expr_return(ret);
    if (!gen_expr(cs, f, h, be->rhs, &rhs_er)) {
      return 0;
    }

    frame_define_target(h, end_target_number, objfile_section_size(objfile_text(f)));
    expr_return_set(f, er, ret);
    return 1;
  } break;
  case AST_BINOP_LOGICAL_AND: {
    struct expr_return lhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, be->lhs, &lhs_er)) {
      return 0;
    }

    struct loc ret = frame_push_loc(h, KIRA_BOOL_SIZE);

    size_t target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, lhs_er.u.loc, target_number);

    struct expr_return rhs_er = demand_expr_return(ret);
    if (!gen_expr(cs, f, h, be->rhs, &rhs_er)) {
      return 0;
    }

    size_t end_target_number = frame_add_target(h);
    gen_placeholder_jmp(f, h, end_target_number);
    frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

    gen_mov(f, ret, lhs_er.u.loc);

    frame_define_target(h, end_target_number, objfile_section_size(objfile_text(f)));

    expr_return_set(f, er, ret);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

void expr_return_immediate(struct objfile *f, struct frame *h,
                           struct expr_return *er,
                           struct immediate imm) {
  switch (er->tag) {
  case EXPR_RETURN_DEMANDED: {
    gen_mov_immediate(f, er->u.loc, imm);
  } break;
  case EXPR_RETURN_OPEN: {
    /* All immediates (right now) are DWORD-sized. */
    struct loc floc = frame_push_loc(h, DWORD_SIZE);
    gen_mov_immediate(f, floc, imm);
    er->u.loc = floc;
  } break;
  case EXPR_RETURN_FREE: {
    er->u.free.tag = EXPR_RETURN_FREE_IMM;
    er->u.free.u.imm = imm;
  } break;
  default:
    UNREACHABLE();
  }
}

int gen_immediate_numeric_literal(struct objfile *f,
                                  struct frame *h,
                                  struct ast_numeric_literal *a,
                                  struct expr_return *er) {
  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED: {
    int32_t value;
    if (!numeric_literal_to_i32(a->digits, a->digits_count, &value)) {
      return 0;
    }

    struct immediate imm;
    imm.tag = IMMEDIATE_I32;
    imm.u.i32 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_NUMERIC_TYPE_UNSIGNED: {
    uint32_t value;
    if (!numeric_literal_to_u32(a->digits, a->digits_count, &value)) {
      return 0;
    }

    struct immediate imm;
    imm.tag = IMMEDIATE_U32;
    imm.u.u32 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

void apply_field_access(struct checkstate *cs,
                        struct objfile *f,
                        struct frame *h,
                        struct loc lhs_loc,
                        struct ast_typeexpr *type,
                        ident_value fieldname,
                        struct expr_return *er) {
  /* Generally speaking: There's no way the field possibly gets a
     padded_size, because the first N fields of two struct types, if
     identical, need to be accessible when they're in a union
     without touching subsequent fields. */
  uint32_t size;
  uint32_t offset;
  kira_field_sizeoffset(&cs->nt, type, fieldname,
                        &size, &offset);

  /* Note: This code needs to preserve lvalues (and it does so). */

  switch (lhs_loc.tag) {
  case LOC_EBP_OFFSET: {
    struct loc field_loc = ebp_loc(size, size,
                                   int32_add(lhs_loc.u.ebp_offset,
                                             uint32_to_int32(offset)));
    expr_return_set(f, er, field_loc);
  } break;
  case LOC_GLOBAL: {
    /* This could probably be implemented more smartly, with
       advanced symbol-making, but who cares. */
    struct loc field_ptr_loc = frame_push_loc(h, DWORD_SIZE);
    x86_gen_mov_reg_stiptr(f, X86_EAX, lhs_loc.u.global_sti);
    x86_gen_lea32(f, X86_EAX, X86_EAX, uint32_to_int32(offset));
    x86_gen_store32(f, X86_EBP, field_ptr_loc.u.ebp_offset, X86_EAX);
    struct loc field_loc = ebp_indirect_loc(size, size,
                                            field_ptr_loc.u.ebp_offset);
    expr_return_set(f, er, field_loc);
  } break;
  case LOC_EBP_INDIRECT: {
    struct loc field_ptr_loc = frame_push_loc(h, DWORD_SIZE);
    x86_gen_load32(f, X86_EAX, X86_EBP, lhs_loc.u.ebp_indirect);
    x86_gen_lea32(f, X86_EAX, X86_EAX, uint32_to_int32(offset));
    x86_gen_store32(f, X86_EBP, field_ptr_loc.u.ebp_offset, X86_EAX);
    struct loc field_loc = ebp_indirect_loc(size, size,
                                            field_ptr_loc.u.ebp_offset);
    expr_return_set(f, er, field_loc);
  } break;
  default:
    UNREACHABLE();
  }
}

int gen_local_field_access(struct checkstate *cs, struct objfile *f,
                           struct frame *h, struct ast_expr *a,
                           struct expr_return *er) {
  struct expr_return lhs_er = open_expr_return();
  if (!gen_expr(cs, f, h, a->u.local_field_access.lhs, &lhs_er)) {
    return 0;
  }

  apply_field_access(cs, f, h, lhs_er.u.loc,
                     &a->u.local_field_access.lhs->info.concrete_type,
                     a->u.local_field_access.fieldname.value, er);
  return 1;
}

int gen_expr(struct checkstate *cs, struct objfile *f,
             struct frame *h, struct ast_expr *a,
             struct expr_return *er) {
  switch (a->tag) {
  case AST_EXPR_NAME: {
    CHECK(a->u.name.info.info_valid);
    struct def_instantiation *inst = a->u.name.info.inst_or_null;
    if (inst) {
      if (inst->value_computed && inst->value.tag == STATIC_VALUE_PRIMITIVE_OP) {
        struct immediate imm;
        imm.tag = IMMEDIATE_PRIMITIVE_OP;
        imm.u.primitive_op = inst->value.u.primitive_op;
        expr_return_immediate(f, h, er, imm);
      } else {
        CHECK(inst->symbol_table_index_computed);
        if (typeexpr_is_func_type(cs->im, &inst->type)) {
          struct immediate imm;
          imm.tag = IMMEDIATE_FUNC;
          imm.u.func_sti = inst->symbol_table_index;
          expr_return_immediate(f, h, er, imm);
        } else {
          uint32_t size = kira_sizeof(&cs->nt, &inst->type);
          /* TODO: Maybe globals' alignment rules are softer. */
          uint32_t padded_size = size;
          struct loc loc = global_loc(size, padded_size,
                                      inst->symbol_table_index);
          expr_return_set(f, er, loc);
        }
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
    return gen_immediate_numeric_literal(f, h, &a->u.numeric_literal, er);
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
  case AST_EXPR_LAMBDA: {
    /* Lambdas should be compiled to global functions.. separately. */
    TODO_IMPLEMENT;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    return gen_local_field_access(cs, f, h, a, er);
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    struct expr_return lhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, a->u.deref_field_access.lhs, &lhs_er)) {
      return 0;
    }

    struct ast_typeexpr *ptr_target;
    if (!view_ptr_target(cs->im, ast_expr_type(a->u.deref_field_access.lhs),
                         &ptr_target)) {
      CRASH("deref field access typechecked on a non-pointer.");
    }

    uint32_t full_size = kira_sizeof(&cs->nt, ptr_target);
    struct expr_return deref_er = open_expr_return();
    apply_dereference(f, h, lhs_er.u.loc, full_size, &deref_er);

    apply_field_access(cs, f, h, deref_er.u.loc, ptr_target,
                       a->u.deref_field_access.fieldname.value, er);
    return 1;
  } break;
  default:
    UNREACHABLE();
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
      uint32_t var_size = kira_sizeof(&cs->nt,
                                      ast_var_statement_type(&s->u.var_statement));
      struct loc var_loc = frame_push_loc(h, var_size);

      if (s->u.var_statement.has_rhs) {
        int32_t saved_offset = frame_save_offset(h);

        struct expr_return er = demand_expr_return(var_loc);
        if (!gen_expr(cs, f, h, s->u.var_statement.rhs_, &er)) {
          return 0;
        }
        frame_restore_offset(h, saved_offset);
      }

      struct vardata vd;
      size_t var_number = h->var_number;
      h->var_number++;
      vardata_init(&vd, s->u.var_statement.decl.name.value,
                   var_number,
                   ast_var_statement_type(&s->u.var_statement),
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
      int32_t saved_offset = frame_save_offset(h);
      struct expr_return er = open_expr_return();
      if (!gen_expr(cs, f, h, s->u.ifthen_statement.condition, &er)) {
        return 0;
      }

      size_t target_number = frame_add_target(h);
      gen_placeholder_jmp_if_false(f, h, er.u.loc, target_number);
      frame_restore_offset(h, saved_offset);

      gen_bracebody(cs, f, h, &s->u.ifthen_statement.thenbody);

      frame_define_target(h, target_number,
                          objfile_section_size(objfile_text(f)));
    } break;
    case AST_STATEMENT_IFTHENELSE: {
      int32_t saved_offset = frame_save_offset(h);
      struct expr_return er = open_expr_return();
      if (!gen_expr(cs, f, h, s->u.ifthenelse_statement.condition, &er)) {
        return 0;
      }

      size_t target_number = frame_add_target(h);
      gen_placeholder_jmp_if_false(f, h, er.u.loc, target_number);
      frame_restore_offset(h, saved_offset);

      gen_bracebody(cs, f, h, &s->u.ifthenelse_statement.thenbody);

      size_t end_target_number = frame_add_target(h);
      gen_placeholder_jmp(f, h, end_target_number);

      frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

      gen_bracebody(cs, f, h, &s->u.ifthenelse_statement.elsebody);

      frame_define_target(h, end_target_number,
                          objfile_section_size(objfile_text(f)));
    } break;
    default:
      UNREACHABLE();
    }
  }

  for (size_t i = 0; i < vars_pushed; i++) {
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
    replace_placeholder_jump(f, jd.jmp_location, td.target_offset);
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

  gen_function_intro(f, &h);
  note_param_locations(cs, &h, a);

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
  case STATIC_VALUE_BYTE: {
    /* TODO: How should we align our global bytes? */
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, inst->symbol_table_index,
                             objfile_section_size(objfile_data(f)));
    uint8_t bytes[4] = { 0 };
    bytes[0] = inst->value.u.byte_value;
    objfile_section_append_raw(objfile_data(f), bytes, 4);
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
