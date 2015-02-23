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

struct expr_return;
struct loc;
struct frame;

#define FIRST_ENUM_TAG_NUMBER 1

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

enum x86_reg16 {
  X86_AX,
  X86_CX,
  X86_DX,
  X86_BX,
  X86_SP,
  X86_BP,
  X86_SI,
  X86_DI,
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
  X86_SETCC_Z = 0x94,
};

/* Only applicable for 0F-prefixed jmp instructions (I think). */
enum x86_jcc {
  X86_JCC_O = 0x80,
  X86_JCC_Z = 0x84,
  X86_JCC_NE = 0x85,
  X86_JCC_S = 0x88,
  X86_JCC_A = 0x87,
  X86_JCC_C = 0x82,
  X86_JCC_G = 0x8F,
  X86_JCC_L = 0x8C,
};

/* Returns true if the reg has a lobyte (also, the register code
happens to identify the lowbyte in a reg or modr/m field). */
int x86_reg_has_lowbyte(enum x86_reg reg) {
  return reg == X86_EAX || reg == X86_ECX || reg == X86_EDX || reg == X86_EBX;
}

void gen_inst_value(struct checkstate *cs, struct objfile *f, struct frame *h,
                    struct def_instantiation *inst, struct expr_return *er);

struct loc gen_field_loc(struct checkstate *cs,
                         struct objfile *f,
                         struct frame *h,
                         struct loc lhs_loc,
                         struct ast_typeexpr *type,
                         ident_value *fieldname_or_null_for_whole_thing);

struct loc gen_array_element_loc(struct checkstate *cs,
                                 struct objfile *f,
                                 struct frame *h,
                                 struct loc src,
                                 struct ast_typeexpr *elem_type,
                                 uint32_t index);

void gen_primitive_op_behavior(struct checkstate *cs,
                               struct objfile *f,
                               struct frame *h,
                               struct primitive_op prim_op,
                               struct ast_typeexpr *arg0_type_or_null,
                               struct ast_typeexpr *return_type);
void x86_gen_call(struct objfile *f, uint32_t func_sti);
void gen_mov_addressof(struct objfile *f, struct loc dest, struct loc loc);
void gen_mov(struct objfile *f, struct loc dest, struct loc src);
void gen_bzero(struct objfile *f, struct loc dest);

void gen_destroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                 struct loc loc, struct ast_typeexpr *type);
void gen_copy(struct checkstate *cs, struct objfile *f, struct frame *h,
              struct loc dest, struct loc src, struct ast_typeexpr *type);
void gen_move_or_copydestroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                             struct loc dest, struct loc src, struct ast_typeexpr *type);
void gen_default_construct(struct checkstate *cs, struct objfile *f, struct frame *h,
                           struct loc dest, struct ast_typeexpr *var_type);
void gen_store_register(struct objfile *f, struct loc dest, enum x86_reg reg);
void gen_load_register(struct objfile *f, enum x86_reg reg, struct loc src);
void gen_crash_jcc(struct objfile *f, struct frame *h, enum x86_jcc code);
void gen_placeholder_jmp(struct objfile *f, struct frame *h, size_t target_number);
void gen_crash_jmp(struct objfile *f, struct frame *h);

/* Right now we don't worry about generating multiple objfiles, so we
just blithely attach a serial number to each name to make them
unique. */
int generate_kira_name(struct checkstate *cs,
                       const void *name, size_t name_count,
                       int is_export,
                       void **gen_name_out, size_t *gen_name_count_out) {
  CHECK(cs->kira_name_counter != UINT32_MAX);
  cs->kira_name_counter++;
  uint32_t number_append = cs->kira_name_counter;
  struct databuf b;
  databuf_init(&b);
  if (is_export) {
    databuf_append(&b, "_", 1);
    databuf_append(&b, name, name_count);
  } else {
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
  }
  databuf_move_destroy(&b, gen_name_out, gen_name_count_out);
  return 1;
}

int is_primitive_but_not_sizeof_alignof(struct def_entry *ent) {
  return ent->is_primitive && ent->primitive_op.tag != PRIMITIVE_OP_SIZEOF
    && ent->primitive_op.tag != PRIMITIVE_OP_ALIGNOF;
}

/* TODO: Put string literals in rdata (add section number to loc_global). */
uint32_t add_data_string(struct checkstate *cs, struct objfile *f,
                         const void *data, uint32_t length) {
  ident_value index = identmap_intern(&cs->sli_values, data, length);
  CHECK(index <= cs->sli_symbol_table_indexes_count);
  if (index == cs->sli_symbol_table_indexes_count) {
    char name[] = "$string_literal";
    void *gen_name;
    size_t gen_name_count;
    if (!generate_kira_name(cs, name, strlen(name),
                            0, &gen_name, &gen_name_count)) {
      return 0;
    }

    uint32_t symbol_table_index
      = objfile_add_local_symbol(f, gen_name, gen_name_count,
                                 objfile_section_size(objfile_data(f)),
                                 SECTION_DATA,
                                 IS_STATIC_YES);

    objfile_section_append_raw(objfile_data(f), data, length);
    SLICE_PUSH(cs->sli_symbol_table_indexes,
               cs->sli_symbol_table_indexes_count,
               cs->sli_symbol_table_indexes_limit,
               symbol_table_index);
  }

  return cs->sli_symbol_table_indexes[index];
}

int add_def_symbols(struct checkstate *cs, struct objfile *f,
                    struct def_entry *ent) {
  /* TODO: I'd actually like to not define symbols for sizeof and
  alignof.  Their values should be inlined. */
  if (is_primitive_but_not_sizeof_alignof(ent)) {
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
      di_set_symbol_table_index(inst, symbol_table_index);
    }

    return 1;
  }

  CHECK(!(ent->is_export && ent->instantiations_count > 1));

  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations[i];

    void *gen_name;
    size_t gen_name_count;
    if (!generate_kira_name(cs, name, name_count,
                            ent->is_export,
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

    di_set_symbol_table_index(inst, symbol_table_index);
  }

  return 1;
}

enum immediate_tag {
  IMMEDIATE_FUNC,
  IMMEDIATE_PRIMITIVE_OP,
  IMMEDIATE_U32,
  IMMEDIATE_I32,
  IMMEDIATE_U8,
  IMMEDIATE_I8,
  IMMEDIATE_VOID,
};

/* TODO: Ugh primitive_op shouldn't be part of this. */
struct immediate {
  enum immediate_tag tag;
  union {
    uint32_t func_sti;
    struct primitive_op primitive_op;
    uint32_t u32;
    int32_t i32;
    uint8_t u8;
    int8_t i8;
  } u;
};

uint32_t immediate_size(struct immediate imm) {
  switch (imm.tag) {
  case IMMEDIATE_FUNC:
    return DWORD_SIZE;
  case IMMEDIATE_PRIMITIVE_OP:
    CRASH("immediate_size on primitive op.");
  case IMMEDIATE_U32:
  case IMMEDIATE_I32:
    return DWORD_SIZE;
  case IMMEDIATE_U8:
  case IMMEDIATE_I8:
    return 1;
  case IMMEDIATE_VOID:
    return 0;
  default:
    UNREACHABLE();
  }
}

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
  /* TODO: Probably we can just remove varnum, now that label/goto is
  gone. */
  struct varnum varnum;

  /* A non-owned reference to the type. */
  struct ast_typeexpr *concrete_type;
  struct loc loc;
};

void vardata_init(struct vardata *vd,
                  ident_value name,
                  struct varnum varnum,
                  struct ast_typeexpr *concrete_type,
                  struct loc loc) {
  vd->name = name;
  vd->varnum = varnum;
  vd->concrete_type = concrete_type;
  vd->loc = loc;
}

void vardata_init_copy(struct vardata *vd,
                       struct vardata *c) {
  *vd = *c;
}

void vardata_destroy(struct vardata *vd) {
  vd->name = IDENT_VALUE_INVALID;
  vd->concrete_type = NULL;
  vd->loc.tag = (enum loc_tag)-1;
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
  /* These are indexed by varnum values, and contain all the variables
  declared within the function. */

  struct vardata *vardata;
  size_t vardata_count;
  size_t vardata_limit;

  struct targetdata *targetdata;
  size_t targetdata_count;
  size_t targetdata_limit;

  struct jmpdata *jmpdata;
  size_t jmpdata_count;
  size_t jmpdata_limit;

  struct reset_esp_data *espdata;
  size_t espdata_count;
  size_t espdata_limit;

  int calling_info_valid;
  size_t arg_count;
  /* True if loc is the "final destination" of the return value,
  because the function uses a hidden return param. */
  int hidden_return_param;
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
  h->vardata = NULL;
  h->vardata_count = 0;
  h->vardata_limit = 0;

  h->targetdata = NULL;
  h->targetdata_count = 0;
  h->targetdata_limit = 0;

  h->jmpdata = NULL;
  h->jmpdata_count = 0;
  h->jmpdata_limit = 0;

  h->espdata = NULL;
  h->espdata_count = 0;
  h->espdata_limit = 0;

  h->calling_info_valid = 0;

  h->return_target_valid = 0;

  h->crash_target_exists = 0;

  h->stack_offset = 0;
  h->min_stack_offset = 0;
}

void frame_destroy(struct frame *h) {
  SLICE_FREE(h->vardata, h->vardata_count, vardata_destroy);
  h->vardata_limit = 0;
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

void frame_specify_calling_info(struct frame *h, size_t arg_count,
                                struct loc loc, int is_return_hidden) {
  CHECK(!h->calling_info_valid);
  h->calling_info_valid = 1;
  h->arg_count = arg_count;
  h->hidden_return_param = is_return_hidden;
  h->return_loc = loc;
}

int frame_hidden_return_param(struct frame *h) {
  CHECK(h->calling_info_valid);
  return h->hidden_return_param;
}

struct loc frame_return_loc(struct frame *h) {
  CHECK(h->calling_info_valid);
  return h->return_loc;
}

size_t frame_arg_count(struct frame *h) {
  CHECK(h->calling_info_valid);
  return h->arg_count;
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

int exists_hidden_return_param(struct checkstate *cs, struct ast_typeexpr *return_type,
                               uint32_t *return_type_size_out) {
  uint32_t return_type_size = kira_sizeof(&cs->nt, return_type);
  *return_type_size_out = return_type_size;
  if (!(return_type_size <= 2 || return_type_size == DWORD_SIZE
        || return_type_size == 2 * DWORD_SIZE)) {
    return 1;
  } else {
    struct typeexpr_traits traits;
    int success = check_typeexpr_traits(cs, return_type, NULL, &traits);
    CHECK(success);
    /* WINDOWS: This ain't C++, and this ain't consistent with the
    Windows calling convention regarding non-pod (for C++03) types. */
    return traits.movable != TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  }
}

/* X86 and maybe WINDOWS-specific calling convention stuff. */
void note_param_locations(struct checkstate *cs, struct frame *h, struct ast_expr *expr) {
  struct ast_typeexpr *type = ast_expr_type(expr);
  size_t args_count = expr->u.lambda.params_count;
  struct ast_typeexpr *return_type
    = expose_func_return_type(cs->im, type, size_add(args_count, 1));

  uint32_t return_type_size;
  int is_return_hidden = exists_hidden_return_param(cs, return_type, &return_type_size);
  int32_t offset = (2 + is_return_hidden) * DWORD_SIZE;

  size_t vars_pushed = 0;

  for (size_t i = 0, e = expr->u.lambda.params_count; i < e; i++) {
    struct ast_typeexpr *param_type = ast_var_info_type(&expr->u.lambda.params[i].var_info);

    uint32_t size = kira_sizeof(&cs->nt, param_type);
    uint32_t padded_size = uint32_ceil_aligned(size, DWORD_SIZE);

    struct loc loc = ebp_loc(size, padded_size, offset);

    struct varnum varnum = ast_var_info_varnum(&expr->u.lambda.params[i].var_info);

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 varnum, param_type, loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    vars_pushed = size_add(vars_pushed, 1);

    offset = int32_add(offset, uint32_to_int32(padded_size));
  }

  if (is_return_hidden) {
    /* I don't know yet if WINDOWS promises padding, so we assume none. */
    struct loc loc = ebp_indirect_loc(return_type_size,
                                      return_type_size,
                                      2 * DWORD_SIZE);
    frame_specify_calling_info(h, vars_pushed, loc, is_return_hidden);
  } else {
    struct loc loc = frame_push_loc(h, return_type_size);
    frame_specify_calling_info(h, vars_pushed, loc, is_return_hidden);
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

void x86_gen_test_regs8(struct objfile *f, enum x86_reg reg1, enum x86_reg reg2) {
  uint8_t b[2];
  b[0] = 0x84;
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
    CRASH("Trying to put a primitive op immediate in memory.");
  } break;
  case IMMEDIATE_U32: {
    /* LITTLEENDIAN etc. */
    objfile_section_append_raw(objfile_text(f), &imm.u.u32, sizeof(uint32_t));
  } break;
  case IMMEDIATE_I32: {
    /* LITTLEENDIAN etc. */
    objfile_section_append_raw(objfile_text(f), &imm.u.i32, sizeof(int32_t));
  } break;
  case IMMEDIATE_U8:
    UNREACHABLE();
  case IMMEDIATE_I8:
    UNREACHABLE();
  case IMMEDIATE_VOID:
    UNREACHABLE();
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

void x86_gen_shl_cl_w16(struct objfile *f, enum x86_reg16 dest) {
  uint8_t b[3];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0x66;
  b[1] = 0xD3;
  b[2] = mod_reg_rm(MOD11, 4, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_shr_cl_w16(struct objfile *f, enum x86_reg16 dest) {
  uint8_t b[3];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0x66;
  b[1] = 0xD3;
  b[2] = mod_reg_rm(MOD11, 5, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_sar_cl_w16(struct objfile *f, enum x86_reg16 dest) {
  uint8_t b[3];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0x66;
  b[1] = 0xD3;
  b[2] = mod_reg_rm(MOD11, 7, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
}

void x86_gen_sar_cl_w8(struct objfile *f, enum x86_reg8 dest) {
  uint8_t b[2];
  /* SHL, SHR, SAR have different reg/opcode fields. */
  b[0] = 0xD2;
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

void x86_gen_add_w16(struct objfile *f, enum x86_reg16 dest, enum x86_reg16 src) {
  uint8_t b[3];
  b[0] = 0x66;
  b[1] = 0x01;
  b[2] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_dxax_mul_w16(struct objfile *f, enum x86_reg16 src) {
  uint8_t b[3];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0x66;
  b[1] = 0xF7;
  b[2] = mod_reg_rm(MOD11, 4, src);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_imul_w16(struct objfile *f, enum x86_reg16 dest, enum x86_reg16 src) {
  uint8_t b[4];
  b[0] = 0x66;
  b[1] = 0x0F;
  b[2] = 0xAF;
  b[3] = mod_reg_rm(MOD11, dest, src);
  objfile_section_append_raw(objfile_text(f), b, 4);
}

void x86_gen_alah_imul_w8(struct objfile *f, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 5, src);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_eaxedx_div_w32(struct objfile *f, enum x86_reg denom) {
  uint8_t b[2];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0xF7;
  b[1] = mod_reg_rm(MOD11, 6, denom);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_axdx_div_w16(struct objfile *f, enum x86_reg16 denom) {
  uint8_t b[3];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0x66;
  b[1] = 0xF7;
  b[2] = mod_reg_rm(MOD11, 6, denom);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_axdx_idiv_w16(struct objfile *f, enum x86_reg16 denom) {
  uint8_t b[3];
  /* MUL, DIV, IDIV have different modr/m opcode. */
  b[0] = 0x66;
  b[1] = 0xF7;
  b[2] = mod_reg_rm(MOD11, 7, denom);
  objfile_section_append_raw(objfile_text(f), b, 3);
}

void x86_gen_alah_idiv_w8(struct objfile *f, enum x86_reg8 denom) {
  uint8_t b[2];
  b[0] = 0xF6;
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

void x86_gen_cmp_w16(struct objfile *f, enum x86_reg16 lhs, enum x86_reg16 rhs) {
  uint8_t b[3];
  b[0] = 0x66;
  b[1] = 0x39;
  b[2] = mod_reg_rm(MOD11, rhs, lhs);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_cmp_reg16_imm16(struct objfile *f, enum x86_reg16 lhs, int16_t imm16) {
  uint8_t b[5];
  b[0] = 0x66;
  b[1] = 0x81;
  b[2] = mod_reg_rm(MOD11, 7, lhs);
  /* LITTLEENDIAN */
  CHECK(sizeof(imm16) == 2);
  memcpy(b + 3, &imm16, sizeof(imm16));
  objfile_section_append_raw(objfile_text(f), b, 5);
}

void x86_gen_cmp_reg8_imm8(struct objfile *f, enum x86_reg8 lhs, int8_t imm8) {
  uint8_t b[3];
  b[0] = 0x80;
  b[1] = mod_reg_rm(MOD11, 7, lhs);
  b[2] = (uint8_t)imm8;
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_not_w8(struct objfile *f, enum x86_reg8 dest) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 2, dest);
  objfile_section_append_raw(objfile_text(f), b, 2);
}

void x86_gen_not_w16(struct objfile *f, enum x86_reg16 dest) {
  uint8_t b[3];
  b[0] = 0x66;
  b[1] = 0xF7;
  b[2] = mod_reg_rm(MOD11, 2, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
}

void x86_gen_not_w32(struct objfile *f, enum x86_reg16 dest) {
  uint8_t b[2];
  b[0] = 0xF7;
  b[1] = mod_reg_rm(MOD11, 2, dest);
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

void x86_gen_sub_w16(struct objfile *f, enum x86_reg16 dest, enum x86_reg16 src) {
  uint8_t b[3];
  b[0] = 0x66;
  b[1] = 0x29;
  b[2] = mod_reg_rm(MOD11, src, dest);
  objfile_section_append_raw(objfile_text(f), b, 3);
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

void x86_gen_mov_mem_imm8(struct objfile *f,
                          enum x86_reg dest,
                          int32_t dest_disp,
                          int8_t imm) {
  uint8_t b[11];
  b[0] = 0xC6;
  size_t count = x86_encode_reg_rm(b + 1, 0, dest, dest_disp);
  CHECK(count <= 9);
  b[1 + count] = (uint8_t)imm;
  objfile_section_append_raw(objfile_text(f), b, 1 + count + 1);
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

void x86_gen_movzx16(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                     int32_t src_disp) {
  uint8_t b[11];
  b[0] = 0x0F;
  b[1] = 0xB7;
  size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 2);
}

void x86_gen_movsx8(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                    int32_t src_disp) {
  uint8_t b[11];
  b[0] = 0x0F;
  b[1] = 0xBE;
  size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 2);
}

void x86_gen_movsx16(struct objfile *f, enum x86_reg dest, enum x86_reg src_addr,
                     int32_t src_disp) {
  uint8_t b[11];
  b[0] = 0x0F;
  b[1] = 0xBF;
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

void x86_gen_store32(struct objfile *f, enum x86_reg dest_addr, int32_t dest_disp,
                     enum x86_reg src) {
  uint8_t b[10];
  b[0] = 0x89;
  size_t count = x86_encode_reg_rm(b + 1, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 1);
}

void x86_gen_store16(struct objfile *f, enum x86_reg dest_addr, int32_t dest_disp,
                     enum x86_reg16 src) {
  uint8_t b[11];
  b[0] = 0x66;
  b[1] = 0x89;
  size_t count = x86_encode_reg_rm(b + 2, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  objfile_section_append_raw(objfile_text(f), b, count + 2);
}

void x86_gen_store8(struct objfile *f, enum x86_reg dest_addr, int32_t dest_disp,
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

enum typetrav_func {
  TYPETRAV_FUNC_DESTROY,
  TYPETRAV_FUNC_COPY,
  TYPETRAV_FUNC_MOVE_OR_COPYDESTROY,
  TYPETRAV_FUNC_DEFAULT_CONSTRUCT,
};

void push_address(struct objfile *f, struct frame *h, struct loc loc) {
  struct loc dest = frame_push_loc(h, DWORD_SIZE);
  gen_mov_addressof(f, dest, loc);
}

void gen_call_imm(struct checkstate *cs, struct objfile *f, struct frame *h,
                  struct immediate imm,
                  struct ast_typeexpr *arg0_type_or_null,
                  struct ast_typeexpr *return_type) {
  switch (imm.tag) {
  case IMMEDIATE_FUNC:
    /* Dupes code with typetrav_call_func. */
    gen_placeholder_stack_adjustment(f, h, 0);
    x86_gen_call(f, imm.u.func_sti);
    gen_placeholder_stack_adjustment(f, h, 1);
    break;
  case IMMEDIATE_PRIMITIVE_OP: {
    gen_primitive_op_behavior(cs, f, h, imm.u.primitive_op, arg0_type_or_null,
                              return_type);
  } break;
  case IMMEDIATE_U32:
    UNREACHABLE();
  case IMMEDIATE_I32:
    UNREACHABLE();
  case IMMEDIATE_U8:
    UNREACHABLE();
  case IMMEDIATE_I8:
    UNREACHABLE();
  case IMMEDIATE_VOID:
    UNREACHABLE();
  default:
    UNREACHABLE();
  }
}

void typetrav_call_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                        struct def_instantiation *inst);

void gen_typetrav_onearg_call(struct checkstate *cs, struct objfile *f, struct frame *h,
                              struct loc loc, struct def_instantiation *inst) {
  int32_t stack_offset = frame_save_offset(h);
  push_address(f, h, loc);
  typetrav_call_func(cs, f, h, inst);
  frame_restore_offset(h, stack_offset);
}

void gen_typetrav_twoarg_call(struct checkstate *cs, struct objfile *f, struct frame *h,
                              struct loc dest, struct loc src, struct def_instantiation *inst) {
  int32_t stack_offset = frame_save_offset(h);
  push_address(f, h, src);
  push_address(f, h, dest);
  typetrav_call_func(cs, f, h, inst);
  frame_restore_offset(h, stack_offset);
}

int gen_typetrav_name_direct(struct checkstate *cs, struct objfile *f, struct frame *h,
                             enum typetrav_func tf, struct loc dest, struct loc src,
                             struct typeexpr_traits *traits,
                             struct typeexpr_trait_instantiations *insts) {
  switch (tf) {
  case TYPETRAV_FUNC_DESTROY: {
    if (traits->copyable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* Destroying a trivial type: Do nothing. */
      return 1;
    }

    if (!insts->destroy_inst) {
      /* No destroy inst.  Therefore, we haven't destroyed the
      object. */
      return 0;
    }

    gen_typetrav_onearg_call(cs, f, h, dest, insts->destroy_inst);
    return 1;
  } break;
  case TYPETRAV_FUNC_COPY: {
    /* TODO: This check can trigger.  Edit: Can it now? */
    CHECK(traits->copyable != TYPEEXPR_TRAIT_LACKED);
    if (traits->copyable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* Copying a trivial type:  Copy it.. trivially. */
      gen_mov(f, dest, src);
      return 1;
    }

    if (!insts->copy_inst) {
      /* No copy inst.  Therefore, we haven't copied the object. */
      return 0;
    }

    gen_typetrav_twoarg_call(cs, f, h, dest, src, insts->copy_inst);
    return 1;
  } break;
  case TYPETRAV_FUNC_MOVE_OR_COPYDESTROY: {
    if (traits->movable == TYPEEXPR_TRAIT_LACKED) {
      CHECK(traits->copyable != TYPEEXPR_TRAIT_LACKED);
      /* Something shouldn't be trivially copyable but not trivially
      movable. */
      CHECK(traits->copyable != TYPEEXPR_TRAIT_TRIVIALLY_HAD);
      CHECK((insts->copy_inst == NULL) == (insts->destroy_inst == NULL));

      if (!(insts->copy_inst && insts->destroy_inst)) {
        /* No copy or destroy inst.  Therefore, we have not
        copy/destroyed the object. */
        return 0;
      }

      gen_typetrav_twoarg_call(cs, f, h, dest, src, insts->copy_inst);
      gen_typetrav_onearg_call(cs, f, h, src, insts->destroy_inst);
      return 1;
    }

    if (traits->movable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* Moving a trivial type:  Move it.. trivially. */
      gen_mov(f, dest, src);
      return 1;
    }

    if (!insts->move_inst) {
      /* No move inst.  Therefore, we haven't moved the object. */
      return 0;
    }

    gen_typetrav_twoarg_call(cs, f, h, dest, src, insts->move_inst);
    return 1;
  } break;
  case TYPETRAV_FUNC_DEFAULT_CONSTRUCT: {
    CHECK(traits->inittible != TYPEEXPR_TRAIT_LACKED);
    if (traits->inittible == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* We don't need to zero the variable but it's... polite? */
      gen_bzero(f, dest);
      return 1;
    }

    if (!insts->init_inst) {
      /* No init inst.  Therefore, we haven't initted the object. */
      return 0;
    }

    gen_typetrav_onearg_call(cs, f, h, dest, insts->init_inst);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

struct loc gen_subobject_loc(struct objfile *f,
                             struct frame *h,
                             struct loc loc,
                             uint32_t size,
                             uint32_t offset);

struct loc make_enum_num_loc(struct objfile *f,
                             struct frame *h,
                             struct loc loc) {
  CHECK(loc.size >= DWORD_SIZE);
  /* All enums start with a DWORD-sized tag. */
  return gen_subobject_loc(f, h, loc, DWORD_SIZE, 0);
}

struct loc make_enum_body_loc(struct objfile *f,
                              struct frame *h,
                              struct loc loc,
                              uint32_t body_size) {
  CHECK(loc.size >= uint32_add(DWORD_SIZE, body_size));
  /* All enums start with a DWORD-sized tag. */
  return gen_subobject_loc(f, h, loc, body_size, DWORD_SIZE);
}

void gen_typetrav_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                       enum typetrav_func tf, struct loc dest, int has_src,
                       struct loc src, struct ast_typeexpr *type);

void gen_typetrav_rhs_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                           enum typetrav_func tf, struct loc dest, int has_src,
                           struct loc src, struct ast_deftype_rhs *rhs) {
  switch (rhs->tag) {
  case AST_DEFTYPE_RHS_TYPE:
    gen_typetrav_func(cs, f, h, tf, dest, has_src, src, &rhs->u.type);
    break;
  case AST_DEFTYPE_RHS_ENUMSPEC: {
    /* TODO: Instead of duplicating this code everywhere, enums should
    have their do_construct, do_copy, do_move functions defined in one
    place and called. */
    int32_t saved_offset = frame_save_offset(h);
    CHECK(tf != TYPETRAV_FUNC_DEFAULT_CONSTRUCT);
    struct loc enum_num_loc;
    struct loc dest_num_loc;
    if (has_src) {
      enum_num_loc = make_enum_num_loc(f, h, src);
      dest_num_loc = make_enum_num_loc(f, h, dest);
    } else {
      enum_num_loc = make_enum_num_loc(f, h, dest);
      dest_num_loc.tag = (enum loc_tag)-1;
    }

    gen_load_register(f, X86_EAX, enum_num_loc);

    /* TODO: This is inefficient if the enum is trivially copyable. */

    size_t end_target = frame_add_target(h);
    size_t next_target = frame_add_target(h);

    STATIC_CHECK(FIRST_ENUM_TAG_NUMBER == 1);
    for (size_t tagnum = 0, e = size_add(1, rhs->u.enumspec.enumfields_count);
         tagnum < e; tagnum++) {
      x86_gen_cmp_imm32(f, X86_EAX, size_to_int32(tagnum));
      gen_placeholder_jcc(f, h, X86_JCC_NE, next_target);
      switch (tf) {
      case TYPETRAV_FUNC_DESTROY:
        /* Do nothing. */
        break;
      case TYPETRAV_FUNC_COPY: /* fallthrough */
      case TYPETRAV_FUNC_MOVE_OR_COPYDESTROY:
        gen_store_register(f, dest_num_loc, X86_EAX);
        break;
      case TYPETRAV_FUNC_DEFAULT_CONSTRUCT:
        /* Unreachable because default construction is a trivial
        operation. */
        UNREACHABLE();
      default:
        UNREACHABLE();
      }

      if (tagnum != 0) {
        uint32_t field_size = kira_sizeof(&cs->nt, &rhs->u.enumspec.enumfields[tagnum - FIRST_ENUM_TAG_NUMBER].type);
        struct loc dest_body_loc = make_enum_body_loc(f, h, dest, field_size);
        struct loc src_body_loc;
        if (has_src) {
          src_body_loc = make_enum_body_loc(f, h, src, field_size);
        } else {
          src_body_loc.tag = (enum loc_tag)-1;
        }

        gen_typetrav_func(cs, f, h, tf, dest_body_loc, has_src,
                          src_body_loc, &rhs->u.enumspec.enumfields[tagnum - FIRST_ENUM_TAG_NUMBER].type);
      }

      gen_placeholder_jmp(f, h, end_target);
      frame_define_target(h, next_target, objfile_section_size(objfile_text(f)));
      next_target = frame_add_target(h);
    }

    gen_crash_jmp(f, h);

    frame_define_target(h, end_target, objfile_section_size(objfile_text(f)));

    frame_restore_offset(h, saved_offset);
  } break;
  default:
    UNREACHABLE();
  }
}

void gen_typetrav_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                       enum typetrav_func tf, struct loc dest, int has_src,
                       struct loc src, struct ast_typeexpr *type) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct typeexpr_traits traits;
    struct typeexpr_trait_instantiations insts;
    int has_rhs;
    struct ast_deftype_rhs rhs;
    int success = check_typeexpr_name_traits(cs, type, NULL, &traits, &insts,
                                             &has_rhs, &rhs);
    CHECK(success);

    if (!gen_typetrav_name_direct(cs, f, h, tf, dest, src, &traits, &insts)) {
      CHECK(has_rhs);
      gen_typetrav_rhs_func(cs, f, h, tf, dest, has_src, src, &rhs);
    }

    if (has_rhs) {
      ast_deftype_rhs_destroy(&rhs);
    }
    return;
  } break;
  case AST_TYPEEXPR_APP: {
    struct typeexpr_traits traits;
    struct typeexpr_trait_instantiations insts;
    int has_rhs;
    struct ast_deftype_rhs rhs;
    int success = check_typeexpr_app_traits(cs, type, NULL, &traits, &insts,
                                            &has_rhs, &rhs);
    CHECK(success);

    if (!gen_typetrav_name_direct(cs, f, h, tf, dest, src, &traits, &insts)) {
      CHECK(has_rhs);
      gen_typetrav_rhs_func(cs, f, h, tf, dest, has_src, src, &rhs);
    }

    if (has_rhs) {
      ast_deftype_rhs_destroy(&rhs);
    }
    return;
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    for (size_t i = 0, e = type->u.structe.fields_count; i < e; i++) {
      int32_t saved_offset = frame_save_offset(h);
      struct loc dest_field_loc = gen_field_loc(cs, f, h, dest, type, &type->u.structe.fields[i].name.value);
      struct loc src_field_loc;
      if (has_src) {
        src_field_loc = gen_field_loc(cs, f, h, src, type, &type->u.structe.fields[i].name.value);
      } else {
        src_field_loc.tag = (enum loc_tag)-1;
      }

      gen_typetrav_func(cs, f, h, tf, dest_field_loc, has_src, src_field_loc, &type->u.structe.fields[i].type);
      frame_restore_offset(h, saved_offset);
    }
    return;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    switch (tf) {
    case TYPETRAV_FUNC_DESTROY:
      break;
    case TYPETRAV_FUNC_COPY: /* fallthrough */
    case TYPETRAV_FUNC_MOVE_OR_COPYDESTROY:
      gen_mov(f, dest, src);
      break;
    case TYPETRAV_FUNC_DEFAULT_CONSTRUCT:
      gen_bzero(f, dest);
      break;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_ARRAY: {
    for (uint32_t i = 0, e = type->u.arraytype.count; i < e; i++) {
      int32_t saved_offset = frame_save_offset(h);
      struct loc dest_element_loc = gen_array_element_loc(cs, f, h, dest, type->u.arraytype.param, i);
      struct loc src_element_loc;
      if (has_src) {
        src_element_loc = gen_array_element_loc(cs, f, h, src, type->u.arraytype.param, i);
      } else {
        src_element_loc.tag = (enum loc_tag)-1;
      }

      gen_typetrav_func(cs, f, h, tf, dest_element_loc, has_src, src_element_loc, type->u.arraytype.param);

      frame_restore_offset(h, saved_offset);
    }
  } break;
  default:
    UNREACHABLE();
  }
}

void gen_destroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                 struct loc loc, struct ast_typeexpr *type) {
  struct loc ignore;
  ignore.tag = (enum loc_tag)-1;
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_DESTROY, loc, 0, ignore, type);
}

void gen_copy(struct checkstate *cs, struct objfile *f, struct frame *h,
              struct loc dest, struct loc src, struct ast_typeexpr *type) {
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_COPY, dest, 1, src, type);
}
void gen_move_or_copydestroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                             struct loc dest, struct loc src, struct ast_typeexpr *type) {
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_MOVE_OR_COPYDESTROY, dest, 1, src, type);
}
void gen_default_construct(struct checkstate *cs, struct objfile *f, struct frame *h,
                           struct loc loc, struct ast_typeexpr *type) {
  struct loc ignore;
  ignore.tag = (enum loc_tag)-1;
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_DEFAULT_CONSTRUCT, loc, 0, ignore, type);
}

void gen_returnloc_funcreturn_convention(struct objfile *f,
                                         int hidden_return_param,
                                         struct loc return_loc) {
  /* return_loc is always in the stack frame (and padded to
  DWORD_SIZE) or via a hidden return param. */
  if (hidden_return_param) {
    CHECK(return_loc.tag == LOC_EBP_INDIRECT);
    x86_gen_load32(f, X86_EAX, X86_EBP, return_loc.u.ebp_indirect);
  } else if (return_loc.size == 0) {
    x86_gen_xor_w32(f, X86_EAX, X86_EAX);
  } else if (return_loc.size <= DWORD_SIZE) {
    CHECK(return_loc.tag == LOC_EBP_OFFSET);
    CHECK(return_loc.padded_size == DWORD_SIZE);
    x86_gen_load32(f, X86_EAX, X86_EBP, return_loc.u.ebp_offset);
  } else {
    CHECK(return_loc.size == 2 * DWORD_SIZE);
    CHECK(return_loc.tag == LOC_EBP_OFFSET);
    CHECK(return_loc.padded_size == 2 * DWORD_SIZE);
    x86_gen_load32(f, X86_EAX, X86_EBP, return_loc.u.ebp_offset);
    x86_gen_load32(f, X86_EDX, X86_EBP, int32_add(return_loc.u.ebp_offset, DWORD_SIZE));
  }
}

void gen_function_exit(struct checkstate *cs, struct objfile *f, struct frame *h) {
  if (h->return_target_valid) {
    frame_define_target(h, h->return_target_number,
                        objfile_section_size(objfile_text(f)));
  }

  CHECK(frame_arg_count(h) == h->vardata_count);
  for (size_t i = 0, e = frame_arg_count(h); i < e; i++) {
    struct vardata *vd = &h->vardata[size_sub(h->vardata_count, 1)];
    gen_destroy(cs, f, h, vd->loc, vd->concrete_type);
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  }

  gen_returnloc_funcreturn_convention(f, frame_hidden_return_param(h),
                                      h->return_loc);

  x86_gen_mov_reg32(f, X86_ESP, X86_EBP);
  x86_gen_pop32(f, X86_EBP);
  x86_gen_ret(f);

  if (h->crash_target_exists) {
    frame_define_target(h, h->crash_target_number,
                        objfile_section_size(objfile_text(f)));
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

void gen_load_addressof(struct objfile *f, enum x86_reg dest, struct loc loc) {
  switch (loc.tag) {
  case LOC_EBP_OFFSET: {
    x86_gen_lea32(f, dest, X86_EBP, loc.u.ebp_offset);
  } break;
  case LOC_GLOBAL: {
    x86_gen_mov_reg_stiptr(f, dest, loc.u.global_sti);
  } break;
  case LOC_EBP_INDIRECT: {
    x86_gen_load32(f, dest, X86_EBP, loc.u.ebp_indirect);
  } break;
  default:
    UNREACHABLE();
  }
}

void gen_mov_addressof(struct objfile *f, struct loc dest, struct loc loc) {
  CHECK(dest.size == DWORD_SIZE);
  gen_load_addressof(f, X86_EAX, loc);
  gen_store_register(f, dest, X86_EAX);
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
ebp) are dest or loc.  It's safe to use this if dest and src point to
the same _exact_ memory location, through different means (or through
the same means). */
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

void gen_mem_bzero(struct objfile *f, enum x86_reg reg, int32_t disp, uint32_t upadded_size) {
  struct immediate imm;
  imm.tag = IMMEDIATE_U32;
  imm.u.u32 = 0;

  int32_t padded_size = uint32_to_int32(upadded_size);
  int32_t n = 0;
  while (n < padded_size) {
    if (padded_size - n >= DWORD_SIZE) {
      x86_gen_mov_mem_imm32(f, reg, int32_add(n, disp), imm);
      n += DWORD_SIZE;
    } else {
      x86_gen_mov_mem_imm8(f, reg, int32_add(n, disp), 0);
      n += 1;
    }
  }
}

void gen_bzero(struct objfile *f, struct loc dest) {
  enum x86_reg reg;
  int32_t disp;
  put_ptr_in_reg(f, dest, X86_EAX, &reg, &disp);

  gen_mem_bzero(f, reg, disp, dest.padded_size);
}

void gen_store_register(struct objfile *f, struct loc dest, enum x86_reg reg) {
  CHECK(dest.size <= DWORD_SIZE);
  CHECK(dest.padded_size == DWORD_SIZE);

  enum x86_reg dest_addr;
  int32_t dest_disp;

  switch (dest.tag) {
    case LOC_EBP_OFFSET:
      dest_addr = X86_EBP;
      dest_disp = dest.u.ebp_offset;
      break;
    case LOC_GLOBAL:
      CRASH("Writing to globals is impossible.");
      break;
    case LOC_EBP_INDIRECT: {
      dest_addr = choose_altreg(reg);
      dest_disp = 0;
      x86_gen_load32(f, dest_addr, X86_EBP, dest.u.ebp_indirect);
    } break;
    default:
      UNREACHABLE();
  }

  switch (dest.size) {
  case DWORD_SIZE:
    x86_gen_store32(f, dest_addr, dest_disp, reg);
    break;
  case 2:
    x86_gen_store16(f, dest_addr, dest_disp, reg);
    break;
  case 1:
    x86_gen_store8(f, dest_addr, dest_disp, reg);
    break;
  default:
    CRASH("not implemented or unreachable.");
  }
}

void gen_load_register(struct objfile *f, enum x86_reg reg, struct loc src) {
  CHECK(src.size <= DWORD_SIZE);

  enum x86_reg src_addr;
  int32_t src_disp;

  switch (src.tag) {
  case LOC_EBP_OFFSET:
    src_addr = X86_EBP;
    src_disp = src.u.ebp_offset;
    break;
  case LOC_GLOBAL:
    x86_gen_mov_reg_stiptr(f, reg, src.u.global_sti);
    src_addr = reg;
    src_disp = 0;
    break;
  case LOC_EBP_INDIRECT:
    x86_gen_load32(f, reg, X86_EBP, src.u.ebp_indirect);
    src_addr = reg;
    src_disp = 0;
    break;
  default:
    UNREACHABLE();
  }

  switch (src.size) {
  case DWORD_SIZE:
    x86_gen_load32(f, reg, src_addr, src_disp);
    break;
  case 2:
    x86_gen_movzx16(f, reg, src_addr, src_disp);
    break;
  case 1:
    x86_gen_movzx8(f, reg, src_addr, src_disp);
    break;
  default:
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

void gen_mov_mem_imm(struct objfile *f, enum x86_reg dest_addr, int32_t dest_disp,
                     struct immediate src) {
  switch (immediate_size(src)) {
  case DWORD_SIZE:
    x86_gen_mov_mem_imm32(f, dest_addr, dest_disp, src);
    break;
  case 1:
    switch (src.tag) {
    case IMMEDIATE_U8:
      x86_gen_mov_mem_imm8(f, dest_addr, dest_disp, (int8_t)src.u.u8);
      break;
    case IMMEDIATE_I8:
      x86_gen_mov_mem_imm8(f, dest_addr, dest_disp, src.u.i8);
      break;
    default:
      UNREACHABLE();
    }
    break;
  case 0:
    CHECK(src.tag == IMMEDIATE_VOID);
    break;
  default:
    UNREACHABLE();
  }
}

void gen_mov_immediate(struct objfile *f, struct loc dest, struct immediate src) {
  CHECK(dest.size == immediate_size(src));

  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    gen_mov_mem_imm(f, X86_EBP, dest.u.ebp_offset, src);
    break;
  case LOC_EBP_INDIRECT:
    x86_gen_load32(f, X86_EAX, X86_EBP, dest.u.ebp_indirect);
    gen_mov_mem_imm(f, X86_EAX, 0, src);
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

/* Returns info as to whether a "temporary" of non-trivial type
exists, and where its true loc is.  This is used, generally speaking,
to describe a loc, and the fact that it's part of a temporary (that
will eventually need to be destroyed or moved from, etc). */
struct temp_return {
  int exists;
  struct loc loc;

  /* ast_typeexpr is an unowned pointer. */
  struct ast_typeexpr *temporary_type;
  int whole_thing;
};

struct temp_return temp_none(void) {
  struct temp_return ret;
  ret.exists = 0;
  return ret;
}

struct temp_return temp_exists_trivial(struct loc loc,
                                       int whole_thing) {
  (void)loc, (void)whole_thing;
  return temp_none();
}

struct temp_return temp_immediate(void) {
  return temp_none();
}

struct temp_return temp_exists(struct loc loc,
                               struct ast_typeexpr *temporary_type,
                               int whole_thing) {
  struct temp_return ret;
  ret.exists = 1;
  ret.loc = loc;
  ret.temporary_type = temporary_type;
  ret.whole_thing = whole_thing;
  return ret;
}

struct temp_return temp_subobject(struct temp_return other) {
  struct temp_return ret = other;
  if (ret.exists) {
    ret.whole_thing = 0;
  }
  return ret;
}

struct expr_return_demand {
  struct loc loc_;
};

struct loc erd_loc(struct expr_return_demand *erd) {
  return erd->loc_;
}

struct expr_return_open {
  int has_loc;
  struct loc loc_;
};

struct loc ero_loc(struct expr_return_open *ero) {
  CHECK(ero->has_loc);
  return ero->loc_;
}

void ero_set_loc(struct expr_return_open *ero, struct loc loc) {
  CHECK(!ero->has_loc);
  ero->has_loc = 1;
  ero->loc_ = loc;
}

/* An expr_return tells how gen_expr should provide the return value. */
enum expr_return_tag {
  /* The location where the data is to be returned is precisely
  specified. */
  EXPR_RETURN_DEMANDED,
  /* The location is not specified -- gen_expr should write the data's
  location to "loc", and preserve lvalues. */
  EXPR_RETURN_OPEN,
  EXPR_RETURN_FREE,
};

struct expr_return {
  enum expr_return_tag tag;
  union {
    struct expr_return_demand demand;
    struct expr_return_open open;
    struct expr_return_free free;
  } u;
  /* gen_expr always writes this value. */
  int has_tr;
  struct temp_return tr_;
};

void er_set_tr(struct expr_return *er, struct temp_return tr) {
  CHECK(!er->has_tr);
  er->has_tr = 1;
  er->tr_ = tr;
}

struct temp_return *er_tr(struct expr_return *er) {
  CHECK(er->has_tr);
  return &er->tr_;
}

void gen_destroy_temp(struct checkstate *cs, struct objfile *f, struct frame *h,
                      struct temp_return tr) {
  if (tr.exists) {
    gen_destroy(cs, f, h, tr.loc, tr.temporary_type);
  }
}

void move_or_copy_temporary_into_loc(struct checkstate *cs, struct objfile *f, struct frame *h,
                                     struct loc dest, struct loc src, struct ast_typeexpr *type,
                                     struct temp_return tr) {
  /* We have to copy the value, unless tr.exists and tr.whole_thing. */
  if (tr.exists && tr.whole_thing) {
    /* (Still possible we have to copy the value because... a move
    constructor does not exist?) */
    gen_move_or_copydestroy(cs, f, h, dest, src, type);
  } else {
    gen_copy(cs, f, h, dest, src, type);
    gen_destroy_temp(cs, f, h, tr);
  }
}

/* The tr argument applies to the temporary in _loc_, the parameter,
and it's not the same as the tr value that ends up in er. */
void expr_return_set(struct checkstate *cs, struct objfile *f, struct frame *h,
                     struct expr_return *er, struct loc loc, struct ast_typeexpr *type,
                     struct temp_return tr) {
  switch (er->tag) {
  case EXPR_RETURN_DEMANDED: {
    move_or_copy_temporary_into_loc(cs, f, h, erd_loc(&er->u.demand), loc, type, tr);
    er_set_tr(er, temp_exists(erd_loc(&er->u.demand), type, 1));
  } break;
  case EXPR_RETURN_OPEN: {
    ero_set_loc(&er->u.open, loc);
    er_set_tr(er, tr);
  } break;
  case EXPR_RETURN_FREE: {
    er->u.free.tag = EXPR_RETURN_FREE_LOC;
    er->u.free.u.loc = loc;
    er_set_tr(er, tr);
  } break;
  default:
    UNREACHABLE();
  }
}

void wipe_temporaries(struct checkstate *cs, struct objfile *f, struct frame *h,
                      struct expr_return *src, struct ast_typeexpr *value_type, struct loc *dest_out) {
  CHECK(src->tag != EXPR_RETURN_DEMANDED);

  struct loc loc;
  switch (src->tag) {
  case EXPR_RETURN_DEMANDED:
    UNREACHABLE();
  case EXPR_RETURN_FREE:
    switch (src->u.free.tag) {
    case EXPR_RETURN_FREE_LOC:
      loc = src->u.free.u.loc;
      break;
    case EXPR_RETURN_FREE_IMM:
      UNREACHABLE();
      break;
    default:
      UNREACHABLE();
    }
    break;
  case EXPR_RETURN_OPEN:
    loc = ero_loc(&src->u.open);
    break;
  default:
    UNREACHABLE();
  }

  if (!er_tr(src)->exists) {
    *dest_out = loc;
    return;
  }

  if (er_tr(src)->whole_thing) {
    *dest_out = loc;
    return;
  }

  struct loc retloc = frame_push_loc(h, loc.size);
  gen_copy(cs, f, h, retloc, loc, value_type);
  gen_destroy(cs, f, h, er_tr(src)->loc, er_tr(src)->temporary_type);
  *dest_out = retloc;
  return;
}

struct expr_return free_expr_return(void) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_FREE;
  ret.has_tr = 0;
  return ret;
}

struct expr_return open_expr_return(void) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_OPEN;
  ret.u.open.has_loc = 0;
  ret.has_tr = 0;
  return ret;
}

struct expr_return demand_expr_return(struct loc loc) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_DEMANDED;
  ret.u.demand.loc_ = loc;
  ret.has_tr = 0;
  return ret;
}

void typetrav_call_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                        struct def_instantiation *inst) {
  /* This would just be crazy. */
  CHECK(!inst->owner->is_primitive);
  /* Dupes checks with gen_inst_value. */
  CHECK(inst->value_computed);
  CHECK(inst->owner->is_extern || inst->owner->is_primitive || inst->typecheck_started);
  CHECK(typeexpr_is_func_type(cs->im, &inst->type));

  /* Dupes code with gen_call_imm. */
  gen_placeholder_stack_adjustment(f, h, 0);
  x86_gen_call(f, di_symbol_table_index(inst));
  gen_placeholder_stack_adjustment(f, h, 1);
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

void gen_cmp16_behavior(struct objfile *f,
                        int32_t off0, int32_t off1,
                        enum x86_setcc setcc_code) {
  x86_gen_movzx16(f, X86_EDX, X86_EBP, off0);
  x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
  x86_gen_cmp_w16(f, X86_DX, X86_CX);
  x86_gen_setcc_b8(f, X86_AL, setcc_code);
  x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
}

void gen_cmp8_behavior(struct objfile *f,
                       int32_t off0, int32_t off1,
                       enum x86_setcc setcc_code) {
  x86_gen_movzx8(f, X86_EDX, X86_EBP, off0);
  x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
  x86_gen_cmp_w32(f, X86_EDX, X86_ECX);
  x86_gen_setcc_b8(f, X86_AL, setcc_code);
  x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
}

void gen_enumconstruct_behavior(struct checkstate *cs,
                                struct objfile *f,
                                struct frame *h,
                                size_t enumconstruct_number,
                                struct ast_typeexpr *arg0_type,
                                struct ast_typeexpr *return_type) {
  CHECK(arg0_type);
  /* We have to actually figure out the calling convention and
  arg/return locations and sizes for this op. */
  int32_t saved_stack_offset = frame_save_offset(h);

  uint32_t arg_size = kira_sizeof(&cs->nt, arg0_type);

  uint32_t return_size;
  int hidden_return_param = exists_hidden_return_param(cs, return_type, &return_size);

  struct loc arg_loc = ebp_loc(arg_size, arg_size,
                               h->stack_offset + (hidden_return_param ? DWORD_SIZE : 0));

  struct loc return_loc;
  if (hidden_return_param) {
    return_loc = ebp_indirect_loc(return_size, return_size, h->stack_offset);
  } else {
    return_loc = frame_push_loc(h, return_size);
  }

  struct loc return_enum_num_loc = make_enum_num_loc(f, h, return_loc);
  struct loc return_enum_body_loc = make_enum_body_loc(f, h, return_loc, arg_size);

  struct immediate enum_num_imm;
  enum_num_imm.tag = IMMEDIATE_U32;
  enum_num_imm.u.u32 = uint32_add(FIRST_ENUM_TAG_NUMBER,
                                  size_to_uint32(enumconstruct_number));
  x86_gen_mov_reg_imm32(f, X86_EAX, enum_num_imm);
  gen_store_register(f, return_enum_num_loc, X86_EAX);

  gen_move_or_copydestroy(cs, f, h, return_enum_body_loc, arg_loc, arg0_type);

  gen_returnloc_funcreturn_convention(f, hidden_return_param, return_loc);
  frame_restore_offset(h, saved_stack_offset);
}

void gen_primitive_op_behavior(struct checkstate *cs,
                               struct objfile *f,
                               struct frame *h,
                               struct primitive_op prim_op,
                               struct ast_typeexpr *arg0_type_or_null,
                               struct ast_typeexpr *return_type) {
  int32_t off0 = h->stack_offset;
  int32_t off1 = int32_add(h->stack_offset, DWORD_SIZE);
  switch (prim_op.tag) {
  case PRIMITIVE_OP_ENUMCONSTRUCT: {
    CHECK(arg0_type_or_null);
    /* Unlike with other ops, our calling convention doesn't involve
    one parameter at off0 and another at off1 (and a return value, if
    any, in registers).  We actually have to figure out whether a
    hidden return parameter is involved. */
    gen_enumconstruct_behavior(cs, f, h, prim_op.u.enumconstruct_number,
                               arg0_type_or_null,
                               return_type);
  } break;
  case PRIMITIVE_OP_INIT: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(cs->im, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = kira_sizeof(&cs->nt, target);
    gen_default_construct(cs, f, h, ebp_indirect_loc(size, size, off0),
                          target);
  } break;
  case PRIMITIVE_OP_COPY: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(cs->im, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = kira_sizeof(&cs->nt, target);
    gen_copy(cs, f, h, ebp_indirect_loc(size, size, off0),
             ebp_indirect_loc(size, size, off1),
             target);
  } break;
  case PRIMITIVE_OP_MOVE: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(cs->im, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = kira_sizeof(&cs->nt, target);
    gen_move_or_copydestroy(cs, f, h, ebp_indirect_loc(size, size, off0),
                            ebp_indirect_loc(size, size, off1),
                            target);
  } break;
  case PRIMITIVE_OP_DESTROY: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(cs->im, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = kira_sizeof(&cs->nt, target);
    gen_destroy(cs, f, h, ebp_indirect_loc(size, size, off0),
                target);
  } break;

  case PRIMITIVE_OP_CONVERT_U8_TO_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_U8_TO_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs8(f, X86_AL, X86_AL);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_U8_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_I16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_I32: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
  } break;

  case PRIMITIVE_OP_CONVERT_I8_TO_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs8(f, X86_AL, X86_AL);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_I8_TO_I8: {
    x86_gen_movsx8(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_I8_TO_U16: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs8(f, X86_AL, X86_AL);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_I8_TO_I16: {
    x86_gen_movsx8(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_I8_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_U32: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs8(f, X86_AL, X86_AL);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
    /* I haven't thought hard about how converting to osize should
    work, but I think sign extending and not failing is the right
    thing.  My opinion might change if we add a signed osize type, in
    which u32 -> osize -> sosize -> i32 might be the unchecking
    conversion (but isn't that gross). */
  case PRIMITIVE_OP_CONVERT_I8_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_I32: {
    x86_gen_movsx8(f, X86_EAX, X86_EBP, off0);
  } break;

  case PRIMITIVE_OP_CONVERT_U16_TO_U8: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0xFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_I8: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7F);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7FFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_I32: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
  } break;

  case PRIMITIVE_OP_CONVERT_I16_TO_U8: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0xFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_I8: {
    x86_gen_movsx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7F);
    gen_crash_jcc(f, h, X86_JCC_G);
    x86_gen_cmp_imm32(f, X86_EAX, -0x80);
    gen_crash_jcc(f, h, X86_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_U16: {
    x86_gen_movsx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs32(f, X86_EAX, X86_EAX);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_I16: {
    x86_gen_movsx16(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I16_TO_U32: {
    x86_gen_movsx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs32(f, X86_EAX, X86_EAX);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I16_TO_I32: {
    x86_gen_movsx16(f, X86_EAX, X86_EBP, off0);
  } break;

  case PRIMITIVE_OP_CONVERT_SIZE_TO_U8: /* fallthrough */
    /* I think (without having thought hard) that converting _from_
    osize should fail if data is _lost_ but not if the osize variable,
    intepreted with the same signedness, has the same value. */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_U8: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_U8: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0xFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_I8: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_I8: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7F);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_U16: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0xFFFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_I16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_I16: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7FFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_SIZE_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_SIZE_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_I32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs32(f, X86_EAX, X86_EAX);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;

  case PRIMITIVE_OP_CONVERT_I32_TO_U8: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0xFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_I8: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_I8: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7F);
    gen_crash_jcc(f, h, X86_JCC_G);
    x86_gen_cmp_imm32(f, X86_EAX, -0x80);
    gen_crash_jcc(f, h, X86_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_U16: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0xFFFF);
    gen_crash_jcc(f, h, X86_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_I16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_I16: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_cmp_imm32(f, X86_EAX, 0x7FFF);
    gen_crash_jcc(f, h, X86_JCC_G);
    x86_gen_cmp_imm32(f, X86_EAX, -0x8000);
    gen_crash_jcc(f, h, X86_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs32(f, X86_EAX, X86_EAX);
    gen_crash_jcc(f, h, X86_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_I32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
  } break;

  case PRIMITIVE_OP_NEGATE_I8: {
    x86_gen_movsx8(f, X86_EAX, X86_EBP, off0);
    /* TODO: For this and the other negations, can't we just check OF
    after the fact?  I missed that in the docs on the first read? */
    /* Crashes if the value is INT8_MIN by subtracting 1 and
    overflowing. */
    x86_gen_cmp_reg8_imm8(f, X86_AL, 1);
    gen_crash_jcc(f, h, X86_JCC_O);
    x86_gen_neg_w32(f, X86_AL);
  } break;
  case PRIMITIVE_OP_NEGATE_I16: {
    x86_gen_movsx16(f, X86_EAX, X86_EBP, off0);
    /* Crashes if the value is INT16_MIN by subtracting 1 and
    overflowing. */
    x86_gen_cmp_reg16_imm16(f, X86_AX, 1);
    gen_crash_jcc(f, h, X86_JCC_O);
    x86_gen_neg_w32(f, X86_EAX);
  } break;
  case PRIMITIVE_OP_NEGATE_I32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    /* Crashes if the value is INT32_MIN by subtracting 1 and
    overflowing. */
    x86_gen_cmp_imm32(f, X86_EAX, 1);
    gen_crash_jcc(f, h, X86_JCC_O);
    x86_gen_neg_w32(f, X86_EAX);
  } break;

  case PRIMITIVE_OP_LOGICAL_NOT: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_test_regs8(f, X86_AL, X86_AL);
    x86_gen_setcc_b8(f, X86_AL, X86_SETCC_Z);
  } break;

  case PRIMITIVE_OP_BIT_NOT_I8: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_not_w8(f, X86_AL);
  } break;
  case PRIMITIVE_OP_BIT_NOT_I16: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_not_w16(f, X86_AX);
  } break;
  case PRIMITIVE_OP_BIT_NOT_I32: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_not_w32(f, X86_EAX);
  } break;

  case PRIMITIVE_OP_EQ_PTR: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_PTR: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_NE);
  } break;

  case PRIMITIVE_OP_ADD_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w8(f, X86_AL, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w8(f, X86_AL, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_mul_w8(f, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_C);
    x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
  } break;
  case PRIMITIVE_OP_DIV_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_div_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
  } break;
  case PRIMITIVE_OP_MOD_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_div_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_mov_reg8(f, X86_AL, X86_AH);
  } break;
  case PRIMITIVE_OP_LT_BOOL: /* fallthrough */
  case PRIMITIVE_OP_LT_U8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_B);
  } break;
  case PRIMITIVE_OP_LE_BOOL: /* fallthrough */
  case PRIMITIVE_OP_LE_U8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_BE);
  } break;
  case PRIMITIVE_OP_GT_BOOL: /* fallthrough */
  case PRIMITIVE_OP_GT_U8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_A);
  } break;
  case PRIMITIVE_OP_GE_BOOL: /* fallthrough */
  case PRIMITIVE_OP_GE_U8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_AE);
  } break;
  case PRIMITIVE_OP_EQ_BOOL: /* fallthrough */
  case PRIMITIVE_OP_EQ_U8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_BOOL: /* fallthrough */
  case PRIMITIVE_OP_NE_U8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_BOOL: /* fallthrough */
  case PRIMITIVE_OP_BIT_XOR_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_BOOL: /* fallthrough */
  case PRIMITIVE_OP_BIT_OR_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_BOOL: /* fallthrough */
  case PRIMITIVE_OP_BIT_AND_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w8(f, X86_AL);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shr_cl_w8(f, X86_AL);
  } break;

  case PRIMITIVE_OP_ADD_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w8(f, X86_AL, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_SUB_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w8(f, X86_AL, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_MUL_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_imul_w8(f, X86_CL);
    gen_crash_jcc(f, h, X86_JCC_O);
    x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
  } break;
  case PRIMITIVE_OP_DIV_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_idiv_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_movzx8_reg8(f, X86_EAX, X86_AL);
  } break;
  case PRIMITIVE_OP_MOD_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_alah_idiv_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_mov_reg8(f, X86_AL, X86_AH);
  } break;
  case PRIMITIVE_OP_LT_I8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_L);
  } break;
  case PRIMITIVE_OP_LE_I8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_LE);
  } break;
  case PRIMITIVE_OP_GT_I8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_G);
  } break;
  case PRIMITIVE_OP_GE_I8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_GE);
  } break;
  case PRIMITIVE_OP_EQ_I8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_I8: {
    gen_cmp8_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w8(f, X86_AL);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_I8: {
    x86_gen_movzx8(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx8(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_sar_cl_w8(f, X86_AL);
  } break;

  case PRIMITIVE_OP_ADD_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w16(f, X86_AX, X86_CX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w16(f, X86_AX, X86_CX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_dxax_mul_w16(f, X86_CX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_DIV_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_axdx_div_w16(f, X86_CX);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_axdx_div_w16(f, X86_CX);
    /* Divide by zero will produce #DE. (I guess.) */
    x86_gen_mov_reg32(f, X86_EAX, X86_EDX);
  } break;
  case PRIMITIVE_OP_LT_U16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_B);
  } break;
  case PRIMITIVE_OP_LE_U16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_BE);
  } break;
  case PRIMITIVE_OP_GT_U16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_A);
  } break;
  case PRIMITIVE_OP_GE_U16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_AE);
  } break;
  case PRIMITIVE_OP_EQ_U16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_E);

  } break;
  case PRIMITIVE_OP_NE_U16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w16(f, X86_AX);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shr_cl_w16(f, X86_AX);
  } break;

  case PRIMITIVE_OP_ADD_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w16(f, X86_AX, X86_CX);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_SUB_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w16(f, X86_AX, X86_CX);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_MUL_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_imul_w16(f, X86_AX, X86_CX);
    gen_crash_jcc(f, h, X86_JCC_O);
  } break;
  case PRIMITIVE_OP_DIV_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_axdx_idiv_w16(f, X86_CX);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_axdx_idiv_w16(f, X86_CX);
    x86_gen_mov_reg32(f, X86_EAX, X86_EDX);
  } break;
  case PRIMITIVE_OP_LT_I16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_L);
  } break;
  case PRIMITIVE_OP_LE_I16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_LE);
  } break;
  case PRIMITIVE_OP_GT_I16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_G);
  } break;
  case PRIMITIVE_OP_GE_I16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_GE);
  } break;
  case PRIMITIVE_OP_EQ_I16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_I16: {
    gen_cmp16_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w16(f, X86_AX);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_I16: {
    x86_gen_movzx16(f, X86_EAX, X86_EBP, off0);
    x86_gen_movzx16(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_sar_cl_w16(f, X86_AX);
  } break;

  case PRIMITIVE_OP_ADD_SIZE: /* fallthrough */
  case PRIMITIVE_OP_ADD_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_SIZE: /* fallthrough */
  case PRIMITIVE_OP_SUB_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w32(f, X86_EAX, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_SIZE: /* fallthrough */
  case PRIMITIVE_OP_MUL_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_eaxedx_mul_w32(f, X86_ECX);
    gen_crash_jcc(f, h, X86_JCC_C);
  } break;
  case PRIMITIVE_OP_DIV_SIZE: /* fallthrough */
  case PRIMITIVE_OP_DIV_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_DIV_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_eaxedx_div_w32(f, X86_ECX);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_SIZE: /* fallthrough */
  case PRIMITIVE_OP_MOD_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_MOD_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EDX, X86_EDX);
    x86_gen_eaxedx_div_w32(f, X86_ECX);
    x86_gen_mov_reg32(f, X86_EAX, X86_EDX);
    /* Modulus by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_LT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_LT_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_LT_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_B);
  } break;
  case PRIMITIVE_OP_LE_SIZE: /* fallthrough */
  case PRIMITIVE_OP_LE_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_LE_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_BE);
  } break;
  case PRIMITIVE_OP_GT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_GT_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_GT_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_A);
  } break;
  case PRIMITIVE_OP_GE_SIZE: /* fallthrough */
  case PRIMITIVE_OP_GE_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_GE_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_AE);
  } break;
  case PRIMITIVE_OP_EQ_SIZE: /* fallthrough */
  case PRIMITIVE_OP_EQ_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_EQ_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_E);
  } break;
  case PRIMITIVE_OP_NE_SIZE: /* fallthrough */
  case PRIMITIVE_OP_NE_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_NE_U32: {
    gen_cmp32_behavior(f, off0, off1, X86_SETCC_NE);
  } break;
  case PRIMITIVE_OP_BIT_XOR_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_XOR_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_XOR_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_xor_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_OR_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_OR_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_OR_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_or_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_AND_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_AND_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_AND_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_and_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_LEFTSHIFT_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shl_cl_w32(f, X86_EAX);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U32: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);

    /* We handle out-of-range rhs, that's all. */
    x86_gen_cmp_imm32(f, X86_ECX, 31);
    gen_crash_jcc(f, h, X86_JCC_A);

    x86_gen_shr_cl_w32(f, X86_EAX);
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

  case PRIMITIVE_OP_ADD_OSIZE: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_add_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_SUB_OSIZE: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_sub_w32(f, X86_EAX, X86_ECX);
  } break;
  case PRIMITIVE_OP_MUL_OSIZE: {
    x86_gen_load32(f, X86_EAX, X86_EBP, off0);
    x86_gen_load32(f, X86_ECX, X86_EBP, off1);
    x86_gen_eaxedx_mul_w32(f, X86_ECX);
  } break;

  default:
    UNREACHABLE();
  }
}

int gen_funcall_expr(struct checkstate *cs, struct objfile *f,
                     struct frame *h, struct ast_expr *a,
                     struct expr_return *er) {
  size_t args_count = a->u.funcall.args_count;

  struct expr_return func_er = free_expr_return();
  /* TODO: We must use exprcatch information to see if we should free
  a temporary. */
  if (!gen_expr(cs, f, h, &a->u.funcall.func->expr, &func_er)) {
    return 0;
  }

  struct ast_typeexpr *return_type = ast_expr_type(a);
  uint32_t return_size;
  int hidden_return_param = exists_hidden_return_param(cs, return_type, &return_size);

  /* X86 */
  struct loc return_loc;
  if (er->tag == EXPR_RETURN_DEMANDED) {
    /* Return locations perhaps must be non-aliasable locations on the
    stack -- this checks that it's a stack location or our callee's
    hidden return param pointer. */
    CHECK(erd_loc(&er->u.demand).tag == LOC_EBP_OFFSET
          || (erd_loc(&er->u.demand).tag == LOC_EBP_INDIRECT
              && erd_loc(&er->u.demand).u.ebp_indirect == 2 * DWORD_SIZE));
    return_loc = erd_loc(&er->u.demand);
  } else {
    return_loc = frame_push_loc(h, return_size);
  }

  int32_t saved_offset = frame_save_offset(h);

  for (size_t i = args_count; i > 0;) {
    i--;

    struct ast_expr *arg = &a->u.funcall.args[i].expr;

    struct loc arg_loc = frame_push_loc(h, kira_sizeof(&cs->nt, ast_expr_type(arg)));

    /* TODO: We must use ast_exprcatch information to force the temporary in arg_loc. */
    /* (Right now, EXPR_RETURN_DEMANDED is known to work this way only
    when it encounters another funcall.) */
    struct expr_return er = demand_expr_return(arg_loc);
    int32_t saved_offset = frame_save_offset(h);
    if (!gen_expr(cs, f, h, arg, &er)) {
      return 0;
    }
    frame_restore_offset(h, saved_offset);
  }

  if (hidden_return_param) {
    struct loc ptr_loc = frame_push_loc(h, DWORD_SIZE);
    gen_mov_addressof(f, ptr_loc, return_loc);
  }

  switch (func_er.u.free.tag) {
  case EXPR_RETURN_FREE_IMM: {
    gen_call_imm(cs, f, h, func_er.u.free.u.imm,
                 args_count == 0 ? NULL : ast_expr_type(&a->u.funcall.args[0].expr),
                 return_type);
  } break;
  case EXPR_RETURN_FREE_LOC: {
    struct loc func_loc;
    wipe_temporaries(cs, f, h, &func_er,
                     ast_expr_type(&a->u.funcall.func->expr), &func_loc);
    gen_load_register(f, X86_EAX, func_loc);
    gen_placeholder_stack_adjustment(f, h, 0);
    x86_gen_indirect_call_reg(f, X86_EAX);
    gen_placeholder_stack_adjustment(f, h, 1);
  } break;
  default:
    UNREACHABLE();
  }

  if (hidden_return_param) {
    if (er->tag != EXPR_RETURN_DEMANDED) {
      /* Let's just pray that the pointer returned by the callee (in
      EAX) is the same as the hidden return param that we passed! */
      expr_return_set(cs, f, h, er, return_loc, return_type,
                      temp_exists(return_loc, return_type, 1));
    } else {
      /* TODO: Icky. */
      er_set_tr(er, temp_exists(erd_loc(&er->u.demand), return_type, 1));
    }
  } else if (return_size == 0) {
    expr_return_set(cs, f, h, er, return_loc, return_type,
                    temp_exists(return_loc, return_type, 1));
  } else if (return_size <= DWORD_SIZE) {
    /* Return value in eax. */
    gen_store_register(f, return_loc, X86_EAX);
    expr_return_set(cs, f, h, er, return_loc, return_type,
                    temp_exists(return_loc, return_type, 1));
  } else {
    CHECK(return_size == 2 * DWORD_SIZE);
    gen_store_biregister(f, return_loc, X86_EAX, X86_EDX);
    expr_return_set(cs, f, h, er, return_loc, return_type,
                    temp_exists(return_loc, return_type, 1));
  }

  frame_restore_offset(h, saved_offset);

  return 1;
}

void apply_dereference(struct checkstate *cs, struct objfile *f,
                       struct frame *h, struct loc ptr_loc,
                       uint32_t pointee_size, struct expr_return *er,
                       struct ast_typeexpr *type) {
  struct loc loc = frame_push_loc(h, DWORD_SIZE);
  gen_mov(f, loc, ptr_loc);
  CHECK(loc.tag == LOC_EBP_OFFSET);

  /* When dereferencing a pointer, we have no info about padding, so
  the padded size is the same as the size. */
  struct loc ret = ebp_indirect_loc(pointee_size, pointee_size,
                                    loc.u.ebp_offset);
  expr_return_set(cs, f, h, er, ret, type, temp_none());
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

    struct loc rhs_loc;
    wipe_temporaries(cs, f, h, &rhs_er, ast_expr_type(ue->rhs), &rhs_loc);

    struct ast_typeexpr *type = ast_expr_type(a);
    uint32_t size = kira_sizeof(&cs->nt, type);
    apply_dereference(cs, f, h, rhs_loc, size, er, type);
    return 1;
  } break;
  case AST_UNOP_ADDRESSOF: {
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, ue->rhs, &rhs_er)) {
      return 0;
    }

    /* There must be no temporary return because then we'd be taking
    the address of a temporary. */
    CHECK(!er_tr(&rhs_er)->exists);

    struct loc ret = frame_push_loc(h, DWORD_SIZE);
    gen_mov_addressof(f, ret, ero_loc(&rhs_er.u.open));
    expr_return_set(cs, f, h, er, ret, ast_expr_type(a),
                    temp_exists(ret, ast_expr_type(a), 1));
    return 1;
  } break;
  case AST_UNOP_NEGATE:
  case AST_UNOP_CONVERT:
  case AST_UNOP_LOGICAL_NOT:
  case AST_UNOP_BITWISE_NOT:
  default:
    UNREACHABLE();
  }
}

int gen_index_expr(struct checkstate *cs, struct objfile *f,
                   struct frame *h, struct ast_expr *a,
                   struct expr_return *er) {
  struct ast_index_expr *ie = &a->u.index_expr;

  struct ast_typeexpr *ptr_target = NULL;
  int is_ptr = view_ptr_target(cs->im, ast_expr_type(ie->lhs), &ptr_target);

  uint32_t elem_size;
  /* Soon becomes dead/alive depending on is_ptr value. */
  struct expr_return lhs_er = open_expr_return();
  if (!gen_expr(cs, f, h, ie->lhs, &lhs_er)) {
    return 0;
  }

  struct loc lhs_loc;
  if (is_ptr) {
    wipe_temporaries(cs, f, h, &lhs_er, ast_expr_type(ie->lhs), &lhs_loc);
  } else {
    lhs_loc = ero_loc(&lhs_er.u.open);
  }

  /* Great, now lhs_er is dead if is_ptr is true and alive if is_ptr
  is false. */

  struct loc rhs_loc;
  {
    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, ie->rhs, &rhs_er)) {
      return 0;
    }
    wipe_temporaries(cs, f, h, &rhs_er, ast_expr_type(ie->rhs), &rhs_loc);
  }

  if (is_ptr) {
    elem_size = kira_sizeof(&cs->nt, ptr_target);

    CHECK(lhs_loc.size == DWORD_SIZE);
    CHECK(rhs_loc.size == DWORD_SIZE);

    gen_load_register(f, X86_EDX, lhs_loc);
  } else {
    struct ast_typeexpr *lhs_type = ast_expr_type(ie->lhs);
    CHECK(lhs_type->tag == AST_TYPEEXPR_ARRAY);
    elem_size = kira_sizeof(&cs->nt, lhs_type->u.arraytype.param);

    CHECK(lhs_loc.size == uint32_mul(elem_size, lhs_type->u.arraytype.count));
    CHECK(rhs_loc.size == DWORD_SIZE);

    gen_load_addressof(f, X86_EDX, lhs_loc);
  }

  gen_load_register(f, X86_EAX, rhs_loc);

  struct immediate imm;
  imm.tag = IMMEDIATE_U32;
  imm.u.u32 = elem_size;
  x86_gen_mov_reg_imm32(f, X86_ECX, imm);
  x86_gen_imul_w32(f, X86_EAX, X86_ECX);
  gen_crash_jcc(f, h, X86_JCC_O);

  x86_gen_add_w32(f, X86_EAX, X86_EDX);

  struct loc loc = frame_push_loc(h, DWORD_SIZE);
  gen_store_register(f, loc, X86_EAX);

  struct loc retloc = ebp_indirect_loc(elem_size, elem_size, loc.u.ebp_offset);
  if (is_ptr) {
    expr_return_set(cs, f, h, er, retloc, ast_expr_type(a), temp_none());
  } else {
    expr_return_set(cs, f, h, er, retloc, ast_expr_type(a), temp_subobject(*er_tr(&lhs_er)));
  }
  return 1;
}

void gen_placeholder_jmp_if_false(struct objfile *f, struct frame *h,
                                  struct loc loc, size_t target_number) {
  CHECK(loc.size == KIRA_BOOL_SIZE);

  gen_load_register(f, X86_EAX, loc);

  x86_gen_test_regs32(f, X86_EAX, X86_EAX);

  gen_placeholder_jcc(f, h, X86_JCC_Z, target_number);
}

size_t frame_crash_target_number(struct frame *h) {
  /* The crash target code gets generated (at the end) only if we call
  this function -- only if h->crash_target_exists is true. */
  if (!h->crash_target_exists) {
    h->crash_target_number = frame_add_target(h);
    h->crash_target_exists = 1;
  }
  return h->crash_target_number;
}

void gen_crash_jcc(struct objfile *f, struct frame *h, enum x86_jcc code) {
  gen_placeholder_jcc(f, h, code, frame_crash_target_number(h));
}

void gen_crash_jmp(struct objfile *f, struct frame *h) {
  gen_placeholder_jmp(f, h, frame_crash_target_number(h));
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

void gen_assignment(struct checkstate *cs, struct objfile *f,
                    struct frame *h, struct loc lhs_loc,
                    struct loc rhs_loc, struct ast_typeexpr *type,
                    struct temp_return rhs_tr) {
  if (loc_equal(lhs_loc, rhs_loc)) {
    /* Statically equal locs, which means self-assignment, also rhs
    can't have a temporary. */
    CHECK(!rhs_tr.exists);
    return;
  }

  struct typeexpr_traits traits;
  int success = check_typeexpr_traits(cs, type, NULL, &traits);
  CHECK(success);

  /* With something trivially copyable we don't have to worry about
  self-assignment or destroying the lhs. */
  if (traits.copyable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
    gen_mov(f, lhs_loc, rhs_loc);
    if (rhs_tr.exists) {
      gen_destroy(cs, f, h, rhs_tr.loc, rhs_tr.temporary_type);
    }
    return;
  }

  size_t target_number = frame_add_target(h);
  gen_load_addressof(f, X86_ECX, lhs_loc);
  gen_load_addressof(f, X86_EDX, rhs_loc);
  x86_gen_cmp_w32(f, X86_EDX, X86_ECX);
  gen_placeholder_jcc(f, h, X86_JCC_Z, target_number);

  /* Okay, memory locations aren't equal. */
  gen_destroy(cs, f, h, lhs_loc, type);
  /* Now move and delete temporary. */
  if (rhs_tr.exists && rhs_tr.whole_thing) {
    gen_move_or_copydestroy(cs, f, h, lhs_loc, rhs_loc, type);
  } else {
    gen_copy(cs, f, h, lhs_loc, rhs_loc, type);
    if (rhs_tr.exists) {
      gen_destroy(cs, f, h, rhs_tr.loc, rhs_tr.temporary_type);
    }
  }

  frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));
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

    /* We are assigning to a value, so it can't be a temporary. */
    CHECK(!er_tr(&lhs_er)->exists);

    struct expr_return rhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, be->rhs, &rhs_er)) {
      return 0;
    }

    struct ast_typeexpr *lhs_type = ast_expr_type(be->lhs);
    gen_assignment(cs, f, h, ero_loc(&lhs_er.u.open), ero_loc(&rhs_er.u.open),
                   lhs_type, *er_tr(&rhs_er));

    /* We have done our assignment.  Our expression has a return
    value though, expr_return_set does that. */
    expr_return_set(cs, f, h, er, ero_loc(&lhs_er.u.open), lhs_type, temp_none());
    return 1;
  } break;
  case AST_BINOP_LOGICAL_OR: {
    struct loc lhs_loc;
    {
      struct expr_return lhs_er = open_expr_return();
      if (!gen_expr(cs, f, h, be->lhs, &lhs_er)) {
        return 0;
      }

      wipe_temporaries(cs, f, h, &lhs_er, ast_expr_type(be->lhs), &lhs_loc);
    }

    struct loc ret = frame_push_loc(h, KIRA_BOOL_SIZE);

    size_t target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, lhs_loc, target_number);
    gen_mov(f, ret, lhs_loc);
    size_t end_target_number = frame_add_target(h);

    gen_placeholder_jmp(f, h, end_target_number);
    frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

    struct expr_return rhs_er = demand_expr_return(ret);
    if (!gen_expr(cs, f, h, be->rhs, &rhs_er)) {
      return 0;
    }

    frame_define_target(h, end_target_number, objfile_section_size(objfile_text(f)));
    expr_return_set(cs, f, h, er, ret, ast_expr_type(a),
                    temp_exists(ret, ast_expr_type(a), 1));
    return 1;
  } break;
  case AST_BINOP_LOGICAL_AND: {
    struct loc lhs_loc;
    {
      struct expr_return lhs_er = open_expr_return();
      if (!gen_expr(cs, f, h, be->lhs, &lhs_er)) {
        return 0;
      }
      wipe_temporaries(cs, f, h, &lhs_er, ast_expr_type(be->lhs), &lhs_loc);
    }

    struct loc ret = frame_push_loc(h, KIRA_BOOL_SIZE);

    size_t target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, lhs_loc, target_number);

    struct expr_return rhs_er = demand_expr_return(ret);
    if (!gen_expr(cs, f, h, be->rhs, &rhs_er)) {
      return 0;
    }

    size_t end_target_number = frame_add_target(h);
    gen_placeholder_jmp(f, h, end_target_number);
    frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

    gen_mov(f, ret, lhs_loc);

    frame_define_target(h, end_target_number, objfile_section_size(objfile_text(f)));

    expr_return_set(cs, f, h, er, ret, ast_expr_type(a),
                    temp_exists(ret, ast_expr_type(a), 1));
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
    gen_mov_immediate(f, erd_loc(&er->u.demand), imm);
    er_set_tr(er, temp_exists_trivial(erd_loc(&er->u.demand), 1));
  } break;
  case EXPR_RETURN_OPEN: {
    struct loc floc = frame_push_loc(h, immediate_size(imm));
    gen_mov_immediate(f, floc, imm);
    ero_set_loc(&er->u.open, floc);
    /* TODO: tr crap like this is bad -- it's a temporary, it should
    be treated as such. */
    er_set_tr(er, temp_exists_trivial(floc, 1));
  } break;
  case EXPR_RETURN_FREE: {
    er->u.free.tag = EXPR_RETURN_FREE_IMM;
    er->u.free.u.imm = imm;
    er_set_tr(er, temp_immediate());
  } break;
  default:
    UNREACHABLE();
  }
}

int help_gen_immediate_numeric(struct identmap *im,
                               struct objfile *f,
                               struct frame *h,
                               struct ast_typeexpr *type,
                               struct ast_meta *meta,
                               uint32_t numeric_literal_value,
                               struct expr_return *er) {
  CHECK(type->tag == AST_TYPEEXPR_NAME);
  CHECK(is_numeric_type(im, type));

  if (type->u.name.value == identmap_intern_c_str(im, I32_TYPE_NAME)) {
    int32_t value;
    if (!squash_u32_to_i32(numeric_literal_value, &value)) {
      return 0;
    }

    struct immediate imm;
    imm.tag = IMMEDIATE_I32;
    imm.u.i32 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } else if (type->u.name.value == identmap_intern_c_str(im, U32_TYPE_NAME)
             || type->u.name.value == identmap_intern_c_str(im, SIZE_TYPE_NAME)
             || type->u.name.value == identmap_intern_c_str(im, OSIZE_TYPE_NAME)) {
    struct immediate imm;
    imm.tag = IMMEDIATE_U32;
    imm.u.u32 = numeric_literal_value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } else if (type->u.name.value == identmap_intern_c_str(im, U8_TYPE_NAME)) {
    uint8_t value;
    if (!squash_u32_to_u8(numeric_literal_value, &value)) {
      return 0;
    }
    struct immediate imm;
    imm.tag = IMMEDIATE_U8;
    imm.u.u8 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } else if (type->u.name.value == identmap_intern_c_str(im, I8_TYPE_NAME)) {
    int8_t value;
    if (!squash_u32_to_i8(numeric_literal_value, &value)) {
      return 0;
    }
    struct immediate imm;
    imm.tag = IMMEDIATE_I8;
    imm.u.i8 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } else {
    METERR(*meta, "Compiler incomplete: Numeric literal resolves to type '%.*s', "
           "which this lame compiler cannot codegen for literals.\n",
           IM_P(im, type->u.name.value));
    return 0;
  }
}

int gen_immediate_numeric_literal(struct identmap *im,
                                  struct objfile *f,
                                  struct frame *h,
                                  struct ast_typeexpr *type,
                                  struct ast_numeric_literal *a,
                                  struct expr_return *er) {
  CHECK(type->tag == AST_TYPEEXPR_NAME);
  CHECK(is_numeric_type(im, type));

  uint32_t numeric_literal_value;
  if (!numeric_literal_to_u32(a, &numeric_literal_value)) {
    return 0;
  }

  return help_gen_immediate_numeric(im, f, h, type, &a->meta,
                                    numeric_literal_value, er);
}

struct loc gen_array_element_loc(struct checkstate *cs,
                                 struct objfile *f,
                                 struct frame *h,
                                 struct loc src,
                                 struct ast_typeexpr *elem_type,
                                 uint32_t index) {
  uint32_t elem_size = kira_sizeof(&cs->nt, elem_type);
  uint32_t elem_offset = uint32_mul(elem_size, index);

  gen_load_addressof(f, X86_EDX, src);
  struct immediate imm;
  imm.tag = IMMEDIATE_U32;
  imm.u.u32 = elem_offset;
  x86_gen_mov_reg_imm32(f, X86_ECX, imm);
  x86_gen_add_w32(f, X86_EDX, X86_ECX);
  struct loc loc = frame_push_loc(h, DWORD_SIZE);
  gen_store_register(f, loc, X86_EDX);

  struct loc retloc = ebp_indirect_loc(elem_size, elem_size, loc.u.ebp_offset);
  return retloc;
}

struct loc gen_field_loc(struct checkstate *cs,
                         struct objfile *f,
                         struct frame *h,
                         struct loc lhs_loc,
                         struct ast_typeexpr *type,
                         ident_value *fieldname_or_null_for_whole_thing) {
  /* Generally speaking: There's no way the field possibly gets a
  padded_size, because the first N fields of two struct types, if
  identical, need to be accessible when they're in a union without
  touching subsequent fields. */
  uint32_t size;
  uint32_t offset;
  if (!fieldname_or_null_for_whole_thing) {
    size = kira_sizeof(&cs->nt, type);
    offset = 0;
  } else {
    kira_field_sizeoffset(&cs->nt, type, *fieldname_or_null_for_whole_thing,
                          &size, &offset);
  }

  return gen_subobject_loc(f, h, lhs_loc, size, offset);
}

struct loc gen_subobject_loc(struct objfile *f,
                             struct frame *h,
                             struct loc loc,
                             uint32_t size,
                             uint32_t offset) {
  CHECK(uint32_add(size, offset) <= loc.size);
  /* Note: This code needs to preserve lvalues (and it does so). */

  struct loc ret;
  switch (loc.tag) {
  case LOC_EBP_OFFSET: {
    ret = ebp_loc(size, size, int32_add(loc.u.ebp_offset,
                                        uint32_to_int32(offset)));
  } break;
  case LOC_GLOBAL: {
    /* This could probably be implemented more smartly, with advanced
    symbol-making, but who cares. */
    struct loc field_ptr_loc = frame_push_loc(h, DWORD_SIZE);
    x86_gen_mov_reg_stiptr(f, X86_EAX, loc.u.global_sti);
    x86_gen_lea32(f, X86_EAX, X86_EAX, uint32_to_int32(offset));
    x86_gen_store32(f, X86_EBP, field_ptr_loc.u.ebp_offset, X86_EAX);
    ret = ebp_indirect_loc(size, size, field_ptr_loc.u.ebp_offset);
  } break;
  case LOC_EBP_INDIRECT: {
    struct loc field_ptr_loc = frame_push_loc(h, DWORD_SIZE);
    x86_gen_load32(f, X86_EAX, X86_EBP, loc.u.ebp_indirect);
    x86_gen_lea32(f, X86_EAX, X86_EAX, uint32_to_int32(offset));
    x86_gen_store32(f, X86_EBP, field_ptr_loc.u.ebp_offset, X86_EAX);
    ret = ebp_indirect_loc(size, size, field_ptr_loc.u.ebp_offset);
  } break;
  default:
    UNREACHABLE();
  }

  return ret;
}

void apply_field_access(struct checkstate *cs,
                        struct objfile *f,
                        struct frame *h,
                        struct loc lhs_loc,
                        struct temp_return lhs_tr,
                        struct ast_typeexpr *type,
                        struct ast_fieldname *fieldname,
                        struct ast_typeexpr *field_type,
                        struct expr_return *er) {
  struct loc field_loc = gen_field_loc(cs, f, h, lhs_loc, type,
                                       fieldname->whole_field ? NULL : &fieldname->ident.value);
  expr_return_set(cs, f, h, er, field_loc, field_type, temp_subobject(lhs_tr));
}

int gen_local_field_access(struct checkstate *cs, struct objfile *f,
                           struct frame *h, struct ast_expr *a,
                           struct expr_return *er) {
  struct expr_return lhs_er = open_expr_return();
  if (!gen_expr(cs, f, h, a->u.local_field_access.lhs, &lhs_er)) {
    return 0;
  }
  struct ast_typeexpr *lhs_type = ast_expr_type(a->u.local_field_access.lhs);
  if (lhs_type->tag == AST_TYPEEXPR_ARRAY) {
    CHECK(!a->u.local_field_access.fieldname.whole_field);
    CHECK(a->u.local_field_access.fieldname.ident.value
          == identmap_intern_c_str(cs->im, ARRAY_LENGTH_FIELDNAME));
    gen_destroy_temp(cs, f, h, *er_tr(&lhs_er));
    struct immediate imm;
    /* X86 32-bit size specific. */
    imm.tag = IMMEDIATE_U32;
    imm.u.u32 = lhs_type->u.arraytype.count;
    expr_return_immediate(f, h, er, imm);
  } else {
    apply_field_access(cs, f, h, ero_loc(&lhs_er.u.open), *er_tr(&lhs_er),
                       lhs_type,
                       &a->u.local_field_access.fieldname,
                       ast_expr_type(a), er);
  }
  return 1;
}

void gen_inst_value(struct checkstate *cs, struct objfile *f, struct frame *h,
                    struct def_instantiation *inst, struct expr_return *er) {
  if (inst->value_computed && di_value(inst)->tag == STATIC_VALUE_PRIMITIVE_OP) {
    struct immediate imm;
    imm.tag = IMMEDIATE_PRIMITIVE_OP;
    imm.u.primitive_op = di_value(inst)->u.primitive_op;
    expr_return_immediate(f, h, er, imm);
  } else {
    CHECK(inst->owner->is_extern || inst->owner->is_primitive || inst->typecheck_started);
    if (typeexpr_is_func_type(cs->im, &inst->type)) {
      struct immediate imm;
      imm.tag = IMMEDIATE_FUNC;
      imm.u.func_sti = di_symbol_table_index(inst);
      expr_return_immediate(f, h, er, imm);
    } else {
      /* TODO: Support immediate values, distinction between defs
      of variables and other defs -- only make a symbol for
      exported defs (if they're small). */
      uint32_t size = kira_sizeof(&cs->nt, &inst->type);
      /* TODO: Maybe globals' alignment rules are softer. */
      uint32_t padded_size = size;
      struct loc loc = global_loc(size, padded_size,
                                  di_symbol_table_index(inst));
      expr_return_set(cs, f, h, er, loc, &inst->type, temp_none());
    }
  }
}

int gen_string_literal(struct checkstate *cs, struct objfile *f,
                       struct frame *h, struct ast_expr *a,
                       struct expr_return *er) {
  uint32_t size = size_to_uint32(a->u.string_literal.values_count);
  uint32_t symbol_table_index
    = add_data_string(cs, f, a->u.string_literal.values, size);

  struct loc loc = global_loc(size, size, symbol_table_index);
  expr_return_set(cs, f, h, er, loc, ast_expr_type(a), temp_none());
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
      gen_inst_value(cs, f, h, inst, er);
    } else {
      /* No template params when looking up a local variable. */
      CHECK(!a->u.name.has_params);
      size_t vi;
      int found_vi = lookup_vardata_by_name(h, a->u.name.ident.value, &vi);
      CHECK(found_vi);
      expr_return_set(cs, f, h, er, h->vardata[vi].loc, h->vardata[vi].concrete_type, temp_none());
    }
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    return gen_immediate_numeric_literal(cs->im, f, h, ast_expr_type(a),
                                         &a->u.numeric_literal, er);
  } break;
  case AST_EXPR_BOOL_LITERAL: {
    CHECK(a->u.bool_literal.value == 0 || a->u.bool_literal.value == 1);
    struct immediate imm;
    imm.tag = IMMEDIATE_U8;
    imm.u.u8 = (uint8_t)a->u.bool_literal.value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_EXPR_NULL_LITERAL: {
    /* Returns an immediate DWORD_SIZE sized value zero for a null
    pointer. */
    STATIC_CHECK(DWORD_SIZE == 4);
    struct immediate imm;
    imm.tag = IMMEDIATE_U32;
    imm.u.u32 = 0;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_EXPR_VOID_LITERAL: {
    struct immediate imm;
    imm.tag = IMMEDIATE_VOID;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_EXPR_CHAR_LITERAL: {
    return help_gen_immediate_numeric(cs->im, f, h, ast_expr_type(a),
                                      &a->u.char_literal.meta,
                                      (uint32_t)a->u.char_literal.value,
                                      er);
  } break;
  case AST_EXPR_STRING_LITERAL: {
    return gen_string_literal(cs, f, h, a, er);
  } break;
  case AST_EXPR_FUNCALL: {
    return gen_funcall_expr(cs, f, h, a, er);
  } break;
  case AST_EXPR_INDEX: {
    return gen_index_expr(cs, f, h, a, er);
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
    struct loc lhs_loc;
    {
      struct expr_return lhs_er = open_expr_return();
      if (!gen_expr(cs, f, h, a->u.deref_field_access.lhs, &lhs_er)) {
        return 0;
      }
      wipe_temporaries(cs, f, h, &lhs_er, ast_expr_type(a->u.deref_field_access.lhs), &lhs_loc);
    }

    struct ast_typeexpr *ptr_target;
    if (!view_ptr_target(cs->im, ast_expr_type(a->u.deref_field_access.lhs),
                         &ptr_target)) {
      CRASH("deref field access typechecked on a non-pointer.");
    }

    uint32_t full_size = kira_sizeof(&cs->nt, ptr_target);
    struct expr_return deref_er = open_expr_return();
    apply_dereference(cs, f, h, lhs_loc, full_size, &deref_er, ptr_target);

    /* An unimportant fact about a check we know. */
    CHECK(!er_tr(&deref_er)->exists);
    apply_field_access(cs, f, h, ero_loc(&deref_er.u.open), *er_tr(&deref_er), ptr_target,
                       &a->u.deref_field_access.fieldname,
                       ast_expr_type(a), er);
    return 1;
  } break;
  case AST_EXPR_TYPED:
    return gen_expr(cs, f, h, a->u.typed_expr.expr, er);
  default:
    UNREACHABLE();
  }
}

void gen_return(struct checkstate *cs, struct objfile *f, struct frame *h) {
  if (!h->return_target_valid) {
    h->return_target_valid = 1;
    h->return_target_number = frame_add_target(h);
  }

  CHECK(h->calling_info_valid);
  for (size_t i = h->vardata_count; i > h->arg_count; ) {
    i--;
    struct vardata *vd = &h->vardata[i];
    gen_destroy(cs, f, h, vd->loc, vd->concrete_type);
  }

  gen_placeholder_jmp(f, h, h->return_target_number);
}

int gen_bracebody(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_bracebody *a);

int gen_statement(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_statement *s,
                  size_t *vars_pushed_ref) {
  switch (s->tag) {
  case AST_STATEMENT_EXPR: {
    int32_t saved_offset = frame_save_offset(h);
    struct expr_return er = open_expr_return();
    if (!gen_expr(cs, f, h, s->u.expr, &er)) {
      return 0;
    }
    gen_destroy_temp(cs, f, h, *er_tr(&er));
    frame_restore_offset(h, saved_offset);
  } break;
  case AST_STATEMENT_RETURN: {
    int32_t saved_offset = frame_save_offset(h);
    struct expr_return er = demand_expr_return(frame_return_loc(h));
    if (s->u.return_statement.has_expr) {
      if (!gen_expr(cs, f, h, s->u.return_statement.expr, &er)) {
        return 0;
      }
    }
    /* No need to generate anything (that assigns to er) for void
    exprs -- the void value is zero-size. */

    gen_return(cs, f, h);
    frame_restore_offset(h, saved_offset);
  } break;
  case AST_STATEMENT_VAR: {
    struct ast_typeexpr *var_type = ast_var_statement_type(&s->u.var_statement);
    uint32_t var_size = kira_sizeof(&cs->nt, var_type);
    struct loc var_loc = frame_push_loc(h, var_size);

    if (s->u.var_statement.has_rhs) {
      int32_t saved_offset = frame_save_offset(h);

      struct expr_return er = demand_expr_return(var_loc);
      /* TODO: Exprcatch information must be used. */
      if (!gen_expr(cs, f, h, &s->u.var_statement.rhs->expr, &er)) {
        return 0;
      }
      /* Do nothing -- er is var_loc, there is nothing to destroy. */
      frame_restore_offset(h, saved_offset);
    } else {
      /* TODO: Var statement explicit initialization instructions must
      be used. */
      gen_default_construct(cs, f, h, var_loc, var_type);
    }

    struct vardata vd;
    struct varnum varnum = ast_var_info_varnum(&s->u.var_statement.decl.var_info);
    vardata_init(&vd, s->u.var_statement.decl.name.value,
                 varnum,
                 ast_var_statement_type(&s->u.var_statement),
                 var_loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);
    (*vars_pushed_ref)++;
  } break;
  case AST_STATEMENT_IFTHEN: {
    int32_t saved_offset = frame_save_offset(h);
    struct loc cond_loc;
    {
      struct expr_return cond_er = open_expr_return();
      if (!gen_expr(cs, f, h, s->u.ifthen_statement.condition, &cond_er)) {
        return 0;
      }
      wipe_temporaries(cs, f, h, &cond_er, ast_expr_type(s->u.ifthen_statement.condition), &cond_loc);
    }

    size_t target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, cond_loc, target_number);
    frame_restore_offset(h, saved_offset);

    if (!gen_bracebody(cs, f, h, &s->u.ifthen_statement.body)) {
      return 0;
    }

    frame_define_target(h, target_number,
                        objfile_section_size(objfile_text(f)));
  } break;
  case AST_STATEMENT_IFTHENELSE: {
    int32_t saved_offset = frame_save_offset(h);
    struct loc cond_loc;
    {
      struct expr_return cond_er = open_expr_return();
      if (!gen_expr(cs, f, h, s->u.ifthenelse_statement.condition, &cond_er)) {
        return 0;
      }
      wipe_temporaries(cs, f, h, &cond_er, ast_expr_type(s->u.ifthenelse_statement.condition), &cond_loc);
    }

    size_t target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, cond_loc, target_number);
    frame_restore_offset(h, saved_offset);

    if (!gen_bracebody(cs, f, h, &s->u.ifthenelse_statement.thenbody)) {
      return 0;
    }

    size_t end_target_number = frame_add_target(h);
    gen_placeholder_jmp(f, h, end_target_number);

    frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

    if (!gen_bracebody(cs, f, h, &s->u.ifthenelse_statement.elsebody)) {
      return 0;
    }

    frame_define_target(h, end_target_number,
                        objfile_section_size(objfile_text(f)));
  } break;
  case AST_STATEMENT_WHILE: {
    size_t top_target_number = frame_add_target(h);
    frame_define_target(h, top_target_number, objfile_section_size(objfile_text(f)));

    int32_t saved_offset = frame_save_offset(h);
    struct loc cond_loc;
    {
      struct expr_return cond_er = open_expr_return();
      if (!gen_expr(cs, f, h, s->u.while_statement.condition, &cond_er)) {
        return 0;
      }
      wipe_temporaries(cs, f, h, &cond_er, ast_expr_type(s->u.while_statement.condition), &cond_loc);
    }

    size_t bottom_target_number = frame_add_target(h);
    gen_placeholder_jmp_if_false(f, h, cond_loc, bottom_target_number);
    frame_restore_offset(h, saved_offset);

    if (!gen_bracebody(cs, f, h, &s->u.while_statement.body)) {
      return 0;
    }

    gen_placeholder_jmp(f, h, top_target_number);
    frame_define_target(h, bottom_target_number, objfile_section_size(objfile_text(f)));
  } break;
  case AST_STATEMENT_FOR: {
    struct ast_for_statement *fs = &s->u.for_statement;
    size_t vars_pushed = 0;
    if (fs->has_initializer) {
      if (!gen_statement(cs, f, h, fs->initializer, &vars_pushed)) {
        return 0;
      }
    }

    size_t top_target_number = frame_add_target(h);
    frame_define_target(h, top_target_number, objfile_section_size(objfile_text(f)));

    size_t bottom_target_number = SIZE_MAX;
    if (fs->has_condition) {
      int32_t saved_offset = frame_save_offset(h);
      struct loc cond_loc;
      {
        struct expr_return cond_er = open_expr_return();
        if (!gen_expr(cs, f, h, fs->condition, &cond_er)) {
          return 0;
        }
        wipe_temporaries(cs, f, h, &cond_er, ast_expr_type(fs->condition), &cond_loc);
      }

      bottom_target_number = frame_add_target(h);
      gen_placeholder_jmp_if_false(f, h, cond_loc, bottom_target_number);
      frame_restore_offset(h, saved_offset);
    }

    if (!gen_bracebody(cs, f, h, &fs->body)) {
      return 0;
    }

    if (fs->has_increment) {
      int32_t saved_offset = frame_save_offset(h);
      struct expr_return er = open_expr_return();
      if (!gen_expr(cs, f, h, fs->increment, &er)) {
        return 0;
      }
      gen_destroy_temp(cs, f, h, *er_tr(&er));
      frame_restore_offset(h, saved_offset);
    }

    gen_placeholder_jmp(f, h, top_target_number);
    if (fs->has_condition) {
      frame_define_target(h, bottom_target_number, objfile_section_size(objfile_text(f)));
    }

    /* TODO: Dedup with code in gen_bracebody. */
    for (size_t i = 0; i < vars_pushed; i++) {
      struct vardata *vd = &h->vardata[size_sub(h->vardata_count, 1)];
      gen_destroy(cs, f, h, vd->loc, vd->concrete_type);
      frame_pop(h, vd->loc.size);
      SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
    }
  } break;
  case AST_STATEMENT_SWITCH: {
    struct ast_switch_statement *ss = &s->u.switch_statement;
    int32_t saved_offset = frame_save_offset(h);

    struct ast_typeexpr *swartch_type = ast_expr_type(ss->swartch);
    struct loc swartch_loc = frame_push_loc(h, kira_sizeof(&cs->nt, swartch_type));

    {
      int32_t swartch_saved_offset = frame_save_offset(h);
      struct expr_return swartch_er = demand_expr_return(swartch_loc);
      if (!gen_expr(cs, f, h, ss->swartch, &swartch_er)) {
        return 0;
      }
      frame_restore_offset(h, swartch_saved_offset);
    }

    struct loc swartch_num_loc = make_enum_num_loc(f, h, swartch_loc);

    gen_load_register(f, X86_EAX, swartch_num_loc);

    size_t end_target = frame_add_target(h);
    size_t next_target = frame_add_target(h);

    for (size_t i = 0, e = ss->cased_statements_count; i < e; i++) {
      int32_t switchcase_saved_offset = frame_save_offset(h);
      struct ast_cased_statement *cas = &ss->cased_statements[i];
      x86_gen_cmp_imm32(f, X86_EAX,
                        int32_add(
                            size_to_int32(
                                ast_case_pattern_info_constructor_number(&cas->pattern.info)),
                            FIRST_ENUM_TAG_NUMBER));
      gen_placeholder_jcc(f, h, X86_JCC_NE, next_target);

      struct ast_typeexpr *var_type = ast_var_info_type(&cas->pattern.decl.var_info);
      struct loc var_loc = make_enum_body_loc(f, h, swartch_loc,
                                              kira_sizeof(&cs->nt, var_type));

      struct vardata vd;
      struct varnum varnum = ast_var_info_varnum(&cas->pattern.decl.var_info);
      vardata_init(&vd, cas->pattern.decl.name.value, varnum, var_type, var_loc);
      SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

      if (!gen_bracebody(cs, f, h, &cas->body)) {
        return 0;
      }

      SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);

      gen_placeholder_jmp(f, h, end_target);
      frame_define_target(h, next_target, objfile_section_size(objfile_text(f)));
      next_target = frame_add_target(h);
      frame_restore_offset(h, switchcase_saved_offset);
    }

    gen_crash_jmp(f, h);

    frame_define_target(h, end_target, objfile_section_size(objfile_text(f)));
    gen_destroy(cs, f, h, swartch_loc, swartch_type);

    frame_restore_offset(h, saved_offset);
  } break;
  default:
    UNREACHABLE();
  }

  return 1;
}

int gen_bracebody(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_bracebody *a) {
  size_t vars_pushed = 0;
  int32_t initial_stack_offset = frame_save_offset(h);

  for (size_t i = 0, e = a->statements_count; i < e; i++) {
    struct ast_statement *s = &a->statements[i];
    if (!gen_statement(cs, f, h, s, &vars_pushed)) {
      return 0;
    }
  }

  for (size_t i = 0; i < vars_pushed; i++) {
    struct vardata *vd = &h->vardata[size_sub(h->vardata_count, 1)];
    gen_destroy(cs, f, h, vd->loc, vd->concrete_type);
    frame_pop(h, vd->loc.size);
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  }

  CHECK(h->stack_offset == initial_stack_offset);
  return 1;
}

void tie_jmps(struct objfile *f, struct frame *h) {
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
    gen_function_exit(cs, f, &h);
    tie_jmps(f, &h);
    tie_stack_adjustments(f, &h);
  }

  frame_destroy(&h);
  return res;
}

int build_instantiation(struct checkstate *cs, struct objfile *f,
                        struct def_instantiation *inst) {
  struct static_value *value = di_value(inst);
  switch (value->tag) {
  case STATIC_VALUE_I32: {
    STATIC_CHECK(sizeof(value->u.i32_value) == 4);
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    objfile_section_append_raw(objfile_data(f),
                               &value->u.i32_value,
                               sizeof(value->u.i32_value));
    return 1;
  } break;
  case STATIC_VALUE_U32: {
    STATIC_CHECK(sizeof(value->u.u32_value) == 4);
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    objfile_section_append_raw(objfile_data(f),
                               &value->u.u32_value,
                               sizeof(value->u.u32_value));
    return 1;
  } break;
  case STATIC_VALUE_U8: {
    /* TODO: How should we align our global bytes? */
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    uint8_t bytes[4] = { 0 };
    bytes[0] = value->u.u8_value;
    objfile_section_append_raw(objfile_data(f), bytes, 4);
    return 1;
  } break;
  case STATIC_VALUE_BOOL: {
    /* TODO: How should we align our global bytes? */
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_Value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    uint8_t bytes[4] = { 0 };
    CHECK(value->u.bool_value == 0 || value->u.bool_value == 1);
    bytes[0] = (uint8_t)value->u.bool_value;
    objfile_section_append_raw(objfile_data(f), bytes, 4);
    return 1;
  } break;
  case STATIC_VALUE_LAMBDA: {
    objfile_fillercode_align_double_quadword(f);
    objfile_set_symbol_Value(f, di_symbol_table_index(inst),
                            objfile_section_size(objfile_text(f)));

    return gen_lambda_expr(cs, f, &value->u.typechecked_lambda);
  } break;
  default:
    UNREACHABLE();
  }
}

int build_def(struct checkstate *cs, struct objfile *f,
              struct def_entry *ent) {
  if (is_primitive_but_not_sizeof_alignof(ent)) {
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
    DBG("(Typecheck failed.)\n");
    goto cleanup_checkstate;
  }

  struct objfile *objfile = NULL;
  objfile_alloc(&objfile);

  for (size_t i = 0, e = cs.nt.defs_count; i < e; i++) {
    if (!add_def_symbols(&cs, objfile, cs.nt.defs[i])) {
      DBG("(Adding def symbol failed.)\n");
      goto cleanup_objfile;
    }
  }

  for (size_t i = 0, e = cs.nt.defs_count; i < e; i++) {
    if (!build_def(&cs, objfile, cs.nt.defs[i])) {
      DBG("(Building def failed.)\n");
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
