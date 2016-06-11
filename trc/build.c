#include "build.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "checkstate.h"
#include "databuf.h"
#include "io.h"
#include "objfile_linux32.h"
#include "objfile_linux64.h"
#include "objfile_objfile.h"
#include "objfile_osx.h"
#include "objfile_win.h"
#include "sizeattr.h"
#include "slice.h"
#include "print.h"
#include "x86.h"

/* TODO(): Change x86 to y86 as code gets ported. */
/* TODO(): Change y86 back to x86 when done. */
/* TODO(): Get rid of "chase mark" comments. */

struct expr_return;
struct loc;
struct frame;

#define FIRST_ENUM_TAG_NUMBER 1

/* Only applicable for 0F-prefixed off32 instructions (I think). */
enum ia_setcc {
  IA_SETCC_L = 0x9C,
  IA_SETCC_LE = 0x9E,
  IA_SETCC_G = 0x9F,
  IA_SETCC_GE = 0x9D,
  IA_SETCC_E = 0x94,
  IA_SETCC_NE = 0x95,
  IA_SETCC_A = 0x97,
  IA_SETCC_AE = 0x93,
  IA_SETCC_B = 0x92,
  IA_SETCC_BE = 0x96,
  IA_SETCC_Z = 0x94,
};

/* Only applicable for 0F-prefixed jmp instructions (I think). */
enum ia_jcc {
  IA_JCC_O = 0x80,
  IA_JCC_Z = 0x84,
  IA_JCC_NE = 0x85,
  IA_JCC_S = 0x88,
  IA_JCC_A = 0x87,
  IA_JCC_AE = 0x83,
  IA_JCC_C = 0x82,
  IA_JCC_G = 0x8F,
  IA_JCC_L = 0x8C,
};

enum oz { OZ_8, OZ_16, OZ_32, OZ_64, };

enum oz ptr_oz(struct objfile *f) {
  switch (objfile_arch(f)) {
  case TARGET_ARCH_Y86:
    return OZ_32;
  case TARGET_ARCH_X64:
    return OZ_64;
  default:
    UNREACHABLE();
  }
}

int ptr_size_bits(struct objfile *f) {
  return 8 << ptr_oz(f);
}

void check_y86x64(struct objfile *f) {
  (void)f;
  /* A no-op -- f->arch must be one of TARGET_ARCH_Y86 or TARGET_ARCH_X64. */
}

void gen_inst_value(struct checkstate *cs, struct objfile *f, struct frame *h,
                    struct def_instantiation *inst, struct expr_return *er);

struct loc gen_field_loc(struct checkstate *cs,
                         struct objfile *f,
                         struct frame *h,
                         struct loc lhs_loc,
                         struct ast_typeexpr *type,
                         ident_value fieldname);

struct loc gen_array_element_loc(struct checkstate *cs,
                                 struct objfile *f,
                                 struct frame *h,
                                 struct loc src,
                                 struct ast_typeexpr *elem_type,
                                 uint32_t index);

struct funcall_arglist_info;
void gen_primitive_op_behavior(struct checkstate *cs,
                               struct objfile *f,
                               struct frame *h,
                               struct primitive_op prim_op,
                               struct ast_typeexpr *arg0_type_or_null,
                               int32_t callsite_base_offset,
                               struct funcall_arglist_info *arglist_info,
                               struct loc return_loc);
void postcall_return_in_loc(struct checkstate *cs,
                            struct objfile *f,
                            struct funcall_arglist_info *arglist_info,
                            struct loc return_loc);
void ia_gen_call(struct objfile *f, struct sti func_sti);
void gen_mov_addressof(struct objfile *f, struct loc dest, struct loc loc);
void gen_mov(struct objfile *f, struct loc dest, struct loc src);
void gen_bzero(struct objfile *f, struct loc dest);

void gen_destroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                 struct loc loc, struct ast_typeexpr *type);
void gen_copy(struct checkstate *cs, struct objfile *f, struct frame *h,
              struct loc dest, struct loc src, struct ast_typeexpr *type);
void gen_move_or_copydestroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                             struct loc dest, struct loc src,
                             struct ast_typeexpr *type);
void gen_default_construct(struct checkstate *cs, struct objfile *f, struct frame *h,
                           struct loc dest, struct ast_typeexpr *var_type);
void gp_gen_store_register(struct objfile *f, struct loc dest, enum gp_reg reg);
void gp_gen_load_register(struct objfile *f, enum gp_reg reg, struct loc src);
void x64_gen_store_register(struct objfile *f, struct loc dest, enum x64_reg reg,
                            enum x64_reg spare);
void x64_gen_load_register(struct objfile *f, enum x64_reg reg, struct loc src);
void gen_crash_jcc(struct objfile *f, struct frame *h, enum ia_jcc code);
void gen_placeholder_jmp(struct objfile *f, struct frame *h, size_t target_number);
void gen_crash_jmp(struct objfile *f, struct frame *h);
void x64_gen_load_addressof(struct objfile *f, enum x64_reg dest, struct loc loc);
void x64_gen_store_biregister(struct objfile *f, struct loc dest,
                              enum x64_reg lo, enum x64_reg hi, enum x64_reg spare);

void apptext(struct objfile *f, const void *buf, size_t count) {
  objfile_section_append_raw(objfile_text(f), buf, count);
}

void pushtext(struct objfile *f, uint8_t byte) {
  apptext(f, &byte, 1);
}

int platform_prefix_underscore(enum target_platform platform) {
  switch (platform) {
  case TARGET_PLATFORM_WIN_32BIT:
    return 1;
  case TARGET_PLATFORM_LINUX_32BIT:
    return 0;
  case TARGET_PLATFORM_LINUX_64BIT:
    return 0;
  case TARGET_PLATFORM_OSX_32BIT:
    return 1;
  case TARGET_PLATFORM_OSX_64BIT:
    /* TODO(): I presume. */
    return 1;
  default:
    UNREACHABLE();
  }
}

void ia_prefix_no_oz8(struct objfile *f, uint8_t opnum, enum oz oz) {
  CHECK(oz != OZ_8);
  if (oz == OZ_16) {
    pushtext(f, 0x66);
  } else if (oz == OZ_64) {
    CHECK(objfile_arch(f) == TARGET_ARCH_X64);
    pushtext(f, kREXW);
  }
  pushtext(f, opnum);
}

void ia_prefix(struct objfile *f, uint8_t opnum, enum oz oz) {
  /* y86/x64 */
  CHECK(opnum & 1);
  if (oz == OZ_8) {
    pushtext(f, opnum ^ 1);
    return;
  }
  ia_prefix_no_oz8(f, opnum, oz);
}

void ia_imm(struct objfile *f, int32_t imm, enum oz oz) {
  uint8_t b[4];
  write_le_i32(b, imm);
  if (oz == OZ_8) {
    CHECK(imm >= -0x80 && imm <= 0xFF);
    apptext(f, b, 1);
  } else if (oz == OZ_16) {
    CHECK(imm >= -0x8000 && imm <= 0xFFFF);
    apptext(f, b, 2);
  } else {
    apptext(f, b, 4);
  }
}

/* Right now we don't worry about generating multiple objfiles, so we
just blithely attach a serial number to each name to make them
unique. */
/* chase mark */
void generate_kit_name(struct checkstate *cs,
                       const void *name, size_t name_count,
                       int is_export,
                       void **gen_name_out, size_t *gen_name_count_out) {
  CHECK(cs->kit_name_counter != UINT32_MAX);
  cs->kit_name_counter++;
  uint32_t number_append = cs->kit_name_counter;
  struct databuf b;
  databuf_init(&b);
  if (is_export) {
    if (platform_prefix_underscore(cs->platform)) {
      databuf_append(&b, "_", 1);
    }
    databuf_append(&b, name, name_count);
  } else {
    if (platform_prefix_underscore(cs->platform)) {
      databuf_append_c_str(&b, "_kit_");
    } else {
      databuf_append_c_str(&b, "kit_");
    }

    /* I just don't want to lookup the stdarg documentation and
    implement databuf_appendf. */
    char buf[30] = { 0 };
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
    databuf_append_c_str(&b, "_");
    databuf_append(&b, name, name_count);
  }
  databuf_move_destroy(&b, gen_name_out, gen_name_count_out);
}

/* TODO: Rename. */
int is_primitive_but_not_sizeof_alignof(struct def_entry *ent) {
  return ent->is_primitive && ent->primitive_op.tag != PRIMITIVE_OP_SIZEOF
    && ent->primitive_op.tag != PRIMITIVE_OP_ALIGNOF
    && ent->primitive_op.tag != PRIMITIVE_OP_ENUMVOID;
}

/* TODO: Put string literals in rdata (add section number to loc_global). */
/* chase mark */
struct sti add_data_string(struct checkstate *cs, struct objfile *f,
                           const void *data, uint32_t length) {
  ident_value index = identmap_intern(&cs->sli_values, data, length);
  CHECK(index <= cs->sli_symbol_table_indexes_count);
  if (index == cs->sli_symbol_table_indexes_count) {
    char name[] = "string_literal$";
    void *gen_name;
    size_t gen_name_count;
    generate_kit_name(cs, name, strlen(name),
                      0, &gen_name, &gen_name_count);

    struct sti symbol_table_index
      = objfile_add_local_symbol(f,
                                 identmap_intern(cs->im, gen_name, gen_name_count),
                                 objfile_section_size(objfile_data(f)),
                                 SECTION_DATA,
                                 IS_STATIC_YES);
    free(gen_name);

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
      if (!objfile_c_symbol_name(cs->platform, name, name_count,
                                 &c_name, &c_name_count)) {
        return 0;
      }

      struct sti symbol_table_index
        = objfile_add_remote_symbol(f, identmap_intern(cs->im, c_name, c_name_count),
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

    struct databuf namebuf;
    databuf_init(&namebuf);
    databuf_append(&namebuf, name, name_count);
    if (inst->owner->generics.has_type_params) {
      sprint_type_param_list(&namebuf, cs->im, inst->substitutions,
                             inst->substitutions_count);
    }

    void *gen_name;
    size_t gen_name_count;
    generate_kit_name(cs, namebuf.buf, namebuf.count,
                      ent->is_export,
                      &gen_name, &gen_name_count);

    databuf_destroy(&namebuf);

    /* We later overwrite the symbol's value (we write zero here). */
    /* No defs are put in a read-only section... for now. */
    struct sti symbol_table_index
      = objfile_add_local_symbol(f, identmap_intern(cs->im, gen_name, gen_name_count),
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
  IMMEDIATE_U64,
  IMMEDIATE_I64,
  IMMEDIATE_U32,
  IMMEDIATE_I32,
  IMMEDIATE_U8,
  IMMEDIATE_I8,
  IMMEDIATE_VOID,
};

struct immediate {
  enum immediate_tag tag;
  union {
    struct sti func_sti;
    uint64_t u64;
    int64_t i64;
    uint32_t u32;
    int32_t i32;
    uint8_t u8;
    int8_t i8;
  } u;
};

/* chase mark */
uint32_t immediate_size(enum target_arch arch, struct immediate imm) {
  switch (imm.tag) {
  case IMMEDIATE_FUNC:
    return ptr_size(arch);
  case IMMEDIATE_U64:
  case IMMEDIATE_I64:
    return 8;
  case IMMEDIATE_U32:
  case IMMEDIATE_I32:
    return 4;
  case IMMEDIATE_U8:
  case IMMEDIATE_I8:
    return 1;
  case IMMEDIATE_VOID:
    return 0;
  default:
    UNREACHABLE();
  }
}

struct immediate imm_u32(uint32_t u32) {
  struct immediate imm;
  imm.tag = IMMEDIATE_U32;
  imm.u.u32 = u32;
  return imm;
}

struct immediate imm_u64(uint64_t u64) {
  struct immediate imm;
  imm.tag = IMMEDIATE_U64;
  imm.u.u64 = u64;
  return imm;
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
    struct sti global_sti;
    int32_t ebp_indirect;
  } u;
};

struct loc ebp_loc(uint32_t size, uint32_t padded_size, int32_t ebp_offset) {
  CHECK(size <= padded_size);
  struct loc ret;
  ret.tag = LOC_EBP_OFFSET;
  ret.size = size;
  ret.padded_size = padded_size;
  ret.u.ebp_offset = ebp_offset;
  return ret;
}

struct loc global_loc(uint32_t size, uint32_t padded_size, struct sti global_sti) {
  CHECK(size <= padded_size);
  struct loc ret;
  ret.tag = LOC_GLOBAL;
  ret.size = size;
  ret.padded_size = padded_size;
  ret.u.global_sti = global_sti;
  return ret;
}

struct loc ebp_indirect_loc(uint32_t size, uint32_t padded_size, int32_t ebp_offset) {
  CHECK(size <= padded_size);
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
    return a.u.global_sti.value == b.u.global_sti.value;
  case LOC_EBP_INDIRECT:
    return a.u.ebp_indirect == b.u.ebp_indirect;
  default:
    UNREACHABLE();
  }
}

struct vardata {
  ident_value name_if_valid;

  /* Variables bound when switching over a pointer-to-enum do not get
  destroyed when unwinding from a return statement. */
  int destroy_when_unwound;

  /* A non-owned reference to the type. */
  struct ast_typeexpr *concrete_type;
  struct loc loc;
};

void vardata_init(struct vardata *vd,
                  ident_value name_if_valid,
                  int destroy_when_unwound,
                  struct ast_typeexpr *concrete_type,
                  struct loc loc) {
  vd->name_if_valid = name_if_valid;
  vd->destroy_when_unwound = destroy_when_unwound;
  vd->concrete_type = concrete_type;
  vd->loc = loc;
}

void vardata_init_copy(struct vardata *vd,
                       struct vardata *c) {
  *vd = *c;
}

void vardata_destroy(struct vardata *vd) {
  vd->name_if_valid = IDENT_VALUE_INVALID;
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
  /* The .text offset where we need to place an addend. */
  size_t reset_esp_offset;
  /* esp's current offset relative to ebp, when we need to reset it.
  (This is a nonpositive value). */
  int32_t ebp_offset;
  /* Says whether we're setting esp to ebp_offset or setting esp back
  from ebp_offset. */
  int downward;
};

struct frame {
  /* Contains all the variables declared within the function. */
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
  below here without harm.

  Function calls (for our x86 ABIs, at least for OS X 32-bit) most
  happen at 16-byte alignment, which means a stack_offset value which
  is 8 (MOD 16), because it's relative to ebp.
  */
  int32_t stack_offset;
  /* min_stack_offset tells us where to put esp.  TODO: Must esp be 8-
  or 16-byte aligned?  For function calls?  Remember that ret ptr
  and ebp take up 8 bytes.  */
  int32_t min_stack_offset;

  /* Same as checkstate's platform, is const and for convenience. */
  enum target_platform platform;
  enum target_arch arch;
};

void frame_init(struct frame *h, enum target_platform platform) {
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

  h->platform = platform;
  h->arch = platform_arch(platform);
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
  h->platform = (enum target_platform)-1;
  h->arch = (enum target_arch)-1;
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

/* chase mark */
size_t frame_add_target(struct frame *h) {
  size_t ret = h->targetdata_count;
  struct targetdata td;
  td.target_known = 0;
  SLICE_PUSH(h->targetdata, h->targetdata_count, h->targetdata_limit, td);
  return ret;
}

/* chase mark */
void frame_define_target(struct frame *h, size_t target_number,
                         uint32_t target_offset) {
  struct targetdata *td = &h->targetdata[target_number];
  CHECK(!td->target_known);
  td->target_known = 1;
  td->target_offset = target_offset;
}

uint32_t frame_padded_push_size(enum target_arch arch, uint32_t size) {
  /* Y86/X64 */
  return uint32_ceil_aligned(size, ptr_size(arch));
}

/* Pushes an exact amount to the frame. */
void frame_push_exact_amount(struct frame *h, uint32_t size) {
  h->stack_offset = int32_sub(h->stack_offset, uint32_to_int32(size));
  if (h->stack_offset < h->min_stack_offset) {
    h->min_stack_offset = h->stack_offset;
  }
}

/* chase mark */
struct loc frame_push_loc(struct frame *h, uint32_t size) {
  /* X86: Make sure new generic padding logic is right for callers on X64. */
  uint32_t padded_size = frame_padded_push_size(h->arch, size);
  frame_push_exact_amount(h, padded_size);
  return ebp_loc(size, padded_size, h->stack_offset);
}

/* chase mark */
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
  uint32_t padded_size = frame_padded_push_size(h->arch, size);
  h->stack_offset = int32_add(h->stack_offset, uint32_to_int32(padded_size));
}

int x64_sysv_memory_param(struct checkstate *cs, struct ast_typeexpr *type,
                          uint32_t *type_size_out) {
  uint32_t size = gp_sizeof(&cs->nt, type);
  *type_size_out = size;
  /* (Are unaligned fields possible?  Like, a struct { u32; { u32;
  u32; }; u32; } might have an unaligned field?  Or is it just the
  inner primitive fields?  Who cares if this is wrong?  This is a
  bootstrapping compiler impl.) */
  if (size > 16) {
    return 1;
  }
  struct typeexpr_traits traits;
  int success = check_typeexpr_traits(cs, type, NULL, &traits);
  CHECK(success);
  return traits.movable != TYPEEXPR_TRAIT_TRIVIALLY_HAD;
}

int exists_hidden_return_param(struct checkstate *cs, struct ast_typeexpr *return_type,
                               uint32_t *return_type_size_out) {
  /* The notion of "hidden return param" is platform-specific
  but... all platforms so far have this notion. */
  struct type_attrs return_type_attrs = gp_attrsof(&cs->nt, return_type);
  switch (cs->platform) {
    /* TODO: OSX isn't quite like Windows -- see the b3sb3 case. */
  case TARGET_PLATFORM_OSX_32BIT: /* fallthrough */
  case TARGET_PLATFORM_WIN_32BIT: {
    uint32_t return_type_size = return_type_attrs.size;
    *return_type_size_out = return_type_size;
    if (!(return_type_size <= 2 || return_type_size == Y86_DWORD_SIZE
          || return_type_size == 2 * Y86_DWORD_SIZE)) {
      return 1;
    } else {
      struct typeexpr_traits traits;
      int success = check_typeexpr_traits(cs, return_type, NULL, &traits);
      CHECK(success);
      /* WINDOWS: This ain't C++, and this ain't consistent with the
      Windows calling convention regarding non-pod (for C++03) types. */
      return traits.movable != TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    }
  } break;
  case TARGET_PLATFORM_LINUX_32BIT: {
    *return_type_size_out = return_type_attrs.size;
    return !return_type_attrs.is_primitive;
  } break;
  case TARGET_PLATFORM_OSX_64BIT: /* fallthrough */
  case TARGET_PLATFORM_LINUX_64BIT: {
    return x64_sysv_memory_param(cs, return_type, return_type_size_out);
  } break;
  default:
    UNREACHABLE();
  }
}

const int32_t Y86_HRP_EBP_DISP = 2 * Y86_DWORD_SIZE;
const int32_t X64_HRP_EBP_DISP = -X64_EIGHTBYTE_SIZE;

/* Returns the ebp_indirect location of the hidden return pointer --
always the same value. */
int64_t hrp_ebp_indirect(enum target_arch arch) {
  switch (arch) {
  case TARGET_ARCH_Y86:
    return Y86_HRP_EBP_DISP;
  case TARGET_ARCH_X64:
    return X64_HRP_EBP_DISP;
  default:
    UNREACHABLE();
  }
}

struct funcall_arg_info {
  /* -1 if the arg will not get put in a register.  Otherwise, the
  "number" of the first register that gets used to pass the argument.
  (It might take up two registers, this value only gets used on x64.)

  On sysv x64, the "number" is from 0 to 5, identifying a register (or
  two) in the sequence {X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8,
  X64_R9}. */
  int first_register;
  /* a negative offset value, if the arg will get put in a register
  before the funcall.  If so, it's in the range [neg_size, 0].  This
  is an actual value that gets used either way, though, to avoid
  unnecessary recomputation. */
  int32_t relative_disp;

  uint32_t arg_size;
  uint32_t padded_size;
};

struct funcall_arglist_info {
  struct funcall_arg_info *arg_infos;
  size_t args_count;
  size_t return_type_size;
  int hidden_return_param;
  uint32_t total_size;
  /* neg_size is -1 times the amount of space used for arguments below
  the call-site.  On the caller side, that's _at_ the callsite.  On
  the callee side, that's below the return pointer, the ebp/rbp
  pointer that we push, _and_ below the HRP or direct return_loc. */
  int32_t neg_size;
};

void funcall_arglist_info_init(struct funcall_arglist_info *a,
                               struct funcall_arg_info *arg_infos,
                               size_t args_count,
                               size_t return_type_size,
                               int hidden_return_param,
                               uint32_t total_size,
                               int32_t neg_size) {
  a->arg_infos = arg_infos;
  a->args_count = args_count;
  a->return_type_size = return_type_size;
  a->hidden_return_param = hidden_return_param;
  a->total_size = total_size;
  a->neg_size = neg_size;
}

void funcall_arglist_info_destroy(struct funcall_arglist_info *a) {
  free(a->arg_infos);
  a->arg_infos = NULL;
  a->args_count = 0;
  a->return_type_size = 0;
  a->hidden_return_param = 0;
  a->total_size = 0;
  a->neg_size = 0;
}

/* Arg's loc from the caller's perspective. (Both non-negative offsets
for "stack params" and non-positive offsets for "register params" are
relative to the same callsite_base_offset.) */
struct loc caller_arg_loc(int32_t callsite_base_offset,
                          struct funcall_arglist_info *fai,
                          size_t arg_number) {
  CHECK(arg_number < fai->args_count);
  struct funcall_arg_info ai = fai->arg_infos[arg_number];
  return ebp_loc(ai.arg_size, ai.padded_size,
                 int32_add(callsite_base_offset, ai.relative_disp));
}

void y86_get_funcall_arglist_info(struct checkstate *cs,
                                  struct ast_typeexpr *return_type,
                                  struct ast_typeexpr *args,
                                  size_t args_count,
                                  struct funcall_arglist_info *info_out);

void x64_get_funcall_arglist_info(struct checkstate *cs,
                                  struct ast_typeexpr *return_type,
                                  struct ast_typeexpr *args,
                                  size_t args_count,
                                  struct funcall_arglist_info *info_out);

void y86_note_param_locations(struct checkstate *cs, struct frame *h,
                              struct ast_expr *expr) {
  struct ast_typeexpr *params;
  size_t params_count;
  struct ast_typeexpr *return_type;
  expose_func_type_parts(&cs->cm, ast_expr_type(expr),
                         &params, &params_count, &return_type);
  CHECK(params_count == expr->u.lambda.params_count);

  struct funcall_arglist_info arglist_info;
  y86_get_funcall_arglist_info(cs, return_type, params, params_count,
                               &arglist_info);

  int32_t ebp_callsite_offset = 2 * Y86_DWORD_SIZE;

  size_t vars_pushed = 0;

  for (size_t i = 0; i < params_count; i++) {
    struct funcall_arg_info arg_info = arglist_info.arg_infos[i];
    CHECK(arg_info.first_register == -1);
    struct loc loc = ebp_loc(arg_info.arg_size, arg_info.padded_size,
                             int32_add(arg_info.relative_disp, ebp_callsite_offset));

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 1, &params[i], loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    vars_pushed = size_add(vars_pushed, 1);
  }

  if (arglist_info.hidden_return_param) {
    /* I don't know if anybody promises padding, so we assume none. */
    struct loc loc = ebp_indirect_loc(arglist_info.return_type_size,
                                      arglist_info.return_type_size,
                                      Y86_HRP_EBP_DISP);
    frame_specify_calling_info(h, vars_pushed, loc,
                               arglist_info.hidden_return_param);
  } else {
    struct loc loc = frame_push_loc(h, arglist_info.return_type_size);
    frame_specify_calling_info(h, vars_pushed, loc,
                               arglist_info.hidden_return_param);
  }

  funcall_arglist_info_destroy(&arglist_info);
}

void x64_gen_store64(struct objfile *f, enum x64_reg dest, int32_t dest_disp,
                     enum x64_reg src);

static const enum x64_reg x64_param_regs[6] = {
  X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9,
};

/* X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, */
void x64_note_param_locations(struct checkstate *cs, struct objfile *f,
                              struct frame *h, struct ast_expr *expr) {
  struct ast_typeexpr *params;
  size_t params_count;
  struct ast_typeexpr *return_type;
  expose_func_type_parts(&cs->cm, ast_expr_type(expr),
                         &params, &params_count, &return_type);
  CHECK(params_count == expr->u.lambda.params_count);

  struct funcall_arglist_info arglist_info;
  x64_get_funcall_arglist_info(cs, return_type, params, params_count,
                               &arglist_info);


  CHECK(h->stack_offset == 0);
  struct loc return_loc;
  if (arglist_info.hidden_return_param) {
    struct loc hrp_loc = frame_push_loc(h, X64_EIGHTBYTE_SIZE);
    CHECK(hrp_loc.u.ebp_offset == X64_HRP_EBP_DISP);
    return_loc = ebp_indirect_loc(arglist_info.return_type_size, arglist_info.return_type_size,
                                  hrp_loc.u.ebp_offset);
    x64_gen_store64(f, X64_RBP, hrp_loc.u.ebp_offset, X64_RDI);
  } else {
    return_loc = frame_push_loc(h, arglist_info.return_type_size);
  }
  int32_t negloc_offset = h->stack_offset;
  frame_push_exact_amount(h, int32_to_uint32(int32_negate(arglist_info.neg_size)));

  /* Memory params start above the stored ebp and return pointer.
  (The HRP comes from a register and is put in memory below ebp, like
  other register params.)  */
  int32_t memory_param_offset = 2 * X64_EIGHTBYTE_SIZE;

  size_t vars_pushed = 0;
  for (size_t i = 0; i < params_count; i++) {
    struct funcall_arg_info ai = arglist_info.arg_infos[i];
    struct loc param_loc;
    if (ai.first_register == -1) {
      CHECK(ai.relative_disp >= 0);
      param_loc = ebp_loc(ai.arg_size, ai.padded_size,
                          int32_add(memory_param_offset, ai.relative_disp));
    } else {
      CHECK(ai.relative_disp <= 0);
      param_loc = ebp_loc(ai.arg_size, ai.padded_size,
                          int32_add(negloc_offset, ai.relative_disp));
      if (ai.arg_size <= 8) {
        CHECK(0 <= ai.first_register && ai.first_register < 6);
        x64_gen_store_register(f, param_loc, x64_param_regs[ai.first_register],
                               X64_RAX);
      } else {
        CHECK(0 <= ai.first_register && ai.first_register < 5);
        x64_gen_store_biregister(f, param_loc,
                                 x64_param_regs[ai.first_register],
                                 x64_param_regs[ai.first_register + 1],
                                 X64_RAX);
      }
    }

    struct vardata vd;
    vardata_init(&vd, expr->u.lambda.params[i].name.value,
                 1, &params[i], param_loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);

    vars_pushed = size_add(vars_pushed, 1);
  }

  frame_specify_calling_info(h, vars_pushed, return_loc,
                             arglist_info.hidden_return_param);

  funcall_arglist_info_destroy(&arglist_info);
}

void note_param_locations(struct checkstate *cs, struct objfile *f, struct frame *h,
                          struct ast_expr *expr) {
  switch (cs->arch) {
  case TARGET_ARCH_Y86: {
    y86_note_param_locations(cs, h, expr);
  } break;
  case TARGET_ARCH_X64: {
    x64_note_param_locations(cs, f, h, expr);
  } break;
  default:
    UNREACHABLE();
  }
}

int lookup_vardata_by_name(struct frame *h, ident_value name, size_t *index_out) {
  /* "Invalid" entries are for unnamed temporaries that need to be
  destroyed when returning -- they aren't unique so it would not make
  sense to look for their name. */
  CHECK(name != IDENT_VALUE_INVALID);
  for (size_t i = 0, e = h->vardata_count; i < e; i++) {
    if (h->vardata[i].name_if_valid == name) {
      *index_out = i;
      return 1;
    }
  }
  return 0;
}

uint8_t mod_reg_rm(int mod, int reg, int rm) {
  CHECK(reg <= 7 && mod <= 3 && rm <= 7);
  return (uint8_t)((mod << 6) | (reg << 3) | rm);
}

#define MOD00 0
#define MOD01 1
#define MOD10 2
#define MOD11 3

void ia_gen_push(struct objfile *f, enum gp_reg reg) {
  CHECK(reg <= GP_DI);
  pushtext(f, 0x50 + (uint8_t)map_x86_reg(reg));
}

void ia_gen_pop(struct objfile *f, enum gp_reg reg) {
  CHECK(reg <= GP_DI);
  pushtext(f, 0x58 + (uint8_t)map_x86_reg(reg));
}

void ia_gen_ret(struct objfile *f) {
  pushtext(f, 0xC3);
}

void x86_gen_retn(struct objfile *f, uint16_t imm16) {
  /* I don't know if the immediate is treated signed or unsigned. */
  CHECK(imm16 < 32768);
  uint8_t b[3];
  b[0] = 0xC2;
  write_le_u16(b + 1, imm16);
  apptext(f, b, 3);
}

void ia_gen_mov(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x8B, oz);
  pushtext(f, mod_reg_rm(MOD11, dest, src));
}

void gp_gen_mov_reg(struct objfile *f, enum gp_reg dest, enum gp_reg src) {
  check_y86x64(f);
  ia_gen_mov(f, dest, src, ptr_oz(f));
}

void ia_gen_mov_reg8(struct objfile *f, enum x86_reg8 dest, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0x8A;
  b[1] = mod_reg_rm(MOD11, dest, src);
  apptext(f, b, 2);
}

void ia_gen_test_regs(struct objfile *f, enum gp_reg reg1, enum gp_reg reg2, enum oz oz) {
  ia_prefix(f, 0x85, oz);
  pushtext(f, mod_reg_rm(MOD11, reg2, reg1));
}

/* Tests the whole reg!!! */
void gp_gen_test_regs(struct objfile *f, enum gp_reg reg1, enum gp_reg reg2, enum oz oz) {
  check_y86x64(f);
  ia_gen_test_regs(f, reg1, reg2, oz);
}

void ia_gen_xor_w32(struct objfile *f, enum gp_reg dest, enum gp_reg src);
void ia_gen_xor(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);

void ia_gen_mov_reg_imm32(struct objfile *f, enum gp_reg dest, int32_t imm32) {
  if (imm32 == 0) {
    ia_gen_xor(f, dest, dest, ptr_oz(f));
  } else {
    CHECK(dest <= GP_DI);
    ia_prefix_no_oz8(f, 0xB8 + (uint8_t)dest, ptr_oz(f));
    uint8_t b[4];
    write_le_i32(b, imm32);
    apptext(f, b, 4);
  }
}

void gp_gen_mov_reg_imm32(struct objfile *f, enum gp_reg dest,
                          int32_t imm32) {
  check_y86x64(f);
  ia_gen_mov_reg_imm32(f, dest, imm32);
}


size_t x86_gen_placeholder_lea32(struct objfile *f, enum x86_reg srcdest);

/* TODO(): Move callers to gp_gen_mov_reg_stiptr */
void x86_gen_mov_reg_stiptr(struct objfile *f, enum x86_reg dest,
                            struct sti symbol_table_index) {
  switch (objfile_platform(f)) {
  case TARGET_PLATFORM_WIN_32BIT: /* fallthrough */
  case TARGET_PLATFORM_LINUX_32BIT: {
    uint8_t b = 0xB8 + (uint8_t)dest;
    apptext(f, &b, 1);
    objfile_section_append_dir32(objfile_text(f), symbol_table_index);
  } break;
  case TARGET_PLATFORM_OSX_32BIT: {
    /* This is how things are done on OS X 32-bit! */
    /* We generate a call/pop pair.  The zero dword is supposed to be
    that way -- the target addr's the next instruction (it's a
    relative address). */
    /* Fortunately we don't have "extern" data decls, that'd be even
    more complicated. */
    uint8_t b[5] = { 0xE8, 0, 0, 0, 0 };
    apptext(f, b, 5);
    size_t subtracted_offset = objfile_section_size(objfile_text(f));
    ia_gen_pop(f, unmap_x86_reg(dest));
    size_t adjusted_offset = x86_gen_placeholder_lea32(f, dest);
    objfile_section_note_diff32(objfile_text(f), symbol_table_index,
                                subtracted_offset, adjusted_offset);
  } break;
  case TARGET_PLATFORM_LINUX_64BIT: {
    TODO_IMPLEMENT;
  } break;
  case TARGET_PLATFORM_OSX_64BIT: {
    TODO_IMPLEMENT;
  } break;
  default:
    UNREACHABLE();
  }
}

void gp_gen_mov_reg_stiptr(struct objfile *f, enum gp_reg dest,
                           struct sti symbol_table_index) {
  switch (objfile_arch(f)) {
  case TARGET_ARCH_Y86:
    x86_gen_mov_reg_stiptr(f, map_x86_reg(dest), symbol_table_index);
    break;
  case TARGET_ARCH_X64:
    TODO_IMPLEMENT;
    break;
  default:
    UNREACHABLE();
  }
}

void x86_gen_int_3(struct objfile *f) {
  pushtext(f, 0xCC);
}

void ia_gen_shl_cl(struct objfile *f, enum gp_reg dest, enum oz oz) {
  ia_prefix(f, 0xD3, oz);
  /* SHL, SHR, SAR have different reg/opcode fields. */
  pushtext(f, mod_reg_rm(MOD11, 4, dest));
}

void ia_gen_shr_cl(struct objfile *f, enum gp_reg dest, enum oz oz) {
  ia_prefix(f, 0xD3, oz);
  /* SHL, SHR, SAR have different reg/opcode fields. */
  pushtext(f, mod_reg_rm(MOD11, 5, dest));
}

void ia_gen_sar_cl(struct objfile *f, enum gp_reg dest, enum oz oz) {
  ia_prefix(f, 0xD3, oz);
  /* SHL, SHR, SAR have different reg/opcode fields. */
  pushtext(f, mod_reg_rm(MOD11, 7, dest));
}

void x86_gen_add_esp_i32(struct objfile *f, int32_t x) {
  uint8_t b[6];
  b[0] = 0x81;
  b[1] = mod_reg_rm(MOD11, 0, X86_ESP);
  write_le_i32(b + 2, x);
  apptext(f, b, 6);
}

void ia_gen_add(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x01, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void ia_gen_azdz_mul(struct objfile *f, enum gp_reg src, enum oz oz) {
  CHECK(oz != OZ_8);
  ia_prefix(f, 0xF7, oz);
  pushtext(f, mod_reg_rm(MOD11, 4, src));
}

void ia_gen_alah_mul_w8(struct objfile *f, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 4, src);
  apptext(f, b, 2);
}

void ia_gen_imul(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  CHECK(oz != OZ_8);
  ia_prefix(f, 0x0F, oz);
  pushtext(f, 0xAF);
  pushtext(f, mod_reg_rm(MOD11, dest, src));
}

void ia_gen_alah_imul_w8(struct objfile *f, enum x86_reg8 src) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 5, src);
  apptext(f, b, 2);
}

void ia_gen_azdz_div(struct objfile *f, enum gp_reg denom, enum oz oz) {
  CHECK(oz != OZ_8);
  ia_prefix(f, 0xF7, oz);
  pushtext(f, mod_reg_rm(MOD11, 6, denom));
}

void ia_gen_alah_div_w8(struct objfile *f, enum x86_reg8 denom) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 6, denom);
  apptext(f, b, 2);
}

void ia_gen_azdz_idiv(struct objfile *f, enum gp_reg denom, enum oz oz) {
  CHECK(oz != OZ_8);
  ia_prefix(f, 0xF7, oz);
  pushtext(f, mod_reg_rm(MOD11, 7, denom));
}

void ia_gen_alah_idiv_w8(struct objfile *f, enum x86_reg8 denom) {
  uint8_t b[2];
  b[0] = 0xF6;
  b[1] = mod_reg_rm(MOD11, 7, denom);
  apptext(f, b, 2);
}

void ia_gen_cwdqo(struct objfile *f, enum oz oz) {
  /* cwd/cdq/cqo */
  CHECK(oz != OZ_8);
  ia_prefix(f, 0x99, oz);
}

void ia_gen_cmp(struct objfile *f, enum gp_reg lhs, enum gp_reg rhs, enum oz oz) {
  ia_prefix(f, 0x39, oz);
  pushtext(f, mod_reg_rm(MOD11, rhs, lhs));
}

void ia_gen_cmp_imm(struct objfile *f, enum gp_reg lhs, int32_t imm, enum oz oz) {
  ia_prefix(f, 0x81, oz);
  pushtext(f, mod_reg_rm(MOD11, 7, lhs));
  ia_imm(f, imm, oz);
}

void ia_gen_xor(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x31, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void ia_gen_or(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x09, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void ia_gen_and(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x21, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void ia_gen_not(struct objfile *f, enum gp_reg dest, enum oz oz) {
  ia_prefix(f, 0xF7, oz);
  pushtext(f, mod_reg_rm(MOD11, 2, dest));
}

void ia_gen_neg(struct objfile *f, enum gp_reg dest, enum oz oz) {
  ia_prefix(f, 0xF7, oz);
  pushtext(f, mod_reg_rm(MOD11, 3, dest));
}

void ia_gen_sub(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x29, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void x64_gen_sub_w64_imm32(struct objfile *f, enum x64_reg dest, int32_t imm) {
  CHECK(dest <= X64_RDI);
  uint8_t b[7];
  b[0] = kREXW;
  b[1] = 0x81;
  b[2] = mod_reg_rm(MOD11, 5, dest);
  write_le_i32(b + 3, imm);
  apptext(f, b, 7);
}

void ia_gen_setcc_b8(struct objfile *f, enum x86_reg8 dest,
                     enum ia_setcc code) {
  uint8_t b[3];
  b[0] = 0x0F;
  b[1] = (uint8_t)code;
  b[2] = mod_reg_rm(MOD11, 0, dest);
  apptext(f, b, 3);
}

/* Either leaves upper bits unset or sets them to zero. */
void gp_gen_setcc_b8(struct objfile *f, enum gp_reg dest, enum ia_setcc code) {
  check_y86x64(f);
  ia_gen_setcc_b8(f, map_x86_reg8(dest), code);
}

/* chase mark */
void gen_placeholder_jcc(struct objfile *f, struct frame *h,
                         enum ia_jcc code, size_t target_number) {
  struct jmpdata jd;
  jd.target_number = target_number;
  /* y86/x64 */
  jd.jmp_location = 2 + objfile_section_size(objfile_text(f));
  SLICE_PUSH(h->jmpdata, h->jmpdata_count, h->jmpdata_limit, jd);

  uint8_t b[6] = { 0x0F, 0, 0, 0, 0, 0 };
  b[1] = code;
  apptext(f, b, 6);
}

/* chase mark */
void gen_placeholder_stack_adjustment(struct objfile *f,
                                      struct frame *h,
                                      int downward) {
  struct reset_esp_data red;
  /* y86/x64 ADD instruction */
  ia_prefix_no_oz8(f, 0x81, ptr_oz(f));
  uint8_t b[5] = { 0 };
  b[0] = mod_reg_rm(MOD11, 0, X86_ESP);

  red.reset_esp_offset = size_add(objfile_section_size(objfile_text(f)), 1);
  red.ebp_offset = h->stack_offset;
  red.downward = downward;
  SLICE_PUSH(h->espdata, h->espdata_count, h->espdata_limit, red);

  apptext(f, b, 5);
}

void replace_placeholder_stack_adjustment(struct objfile *f,
                                          size_t location,
                                          int32_t stack_adjustment) {
  char buf[4];
  write_le_i32(buf, stack_adjustment);
  objfile_section_overwrite_raw(objfile_text(f),
                                location,
                                buf,
                                sizeof(buf));
}

/* Check callers if max count returned increases. */
size_t x86_encode_reg_rm(uint8_t *b, int reg, int rm_addr,
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
    write_le_i32(b + 1, rm_addr_disp);
    return 5;
  }
}

size_t x86_encode_placeholder_reg_rm(uint8_t *b, enum x86_reg reg_and_rm_addr,
                                     size_t *reloc_offset_out) {
  if (reg_and_rm_addr == X86_ESP) {
    CRASH("esp for placeholder_reg_rm not supported");
  } else {
    b[0] = mod_reg_rm(MOD10, reg_and_rm_addr, reg_and_rm_addr);
    write_le_i32(b + 1, 0);
    *reloc_offset_out = 1;
    return 5;
  }
}

void ia_help_gen_mov_mem_imm32(struct objfile *f,
                               enum gp_reg dest,
                               int32_t dest_disp,
                               char buf[4]) {
  check_y86x64(f);
  uint8_t b[10];
  b[0] = 0xC7;
  size_t count = x86_encode_reg_rm(b + 1, 0, map_x86_reg(dest), dest_disp);
  CHECK(count <= 9);
  apptext(f, b, count + 1);
  apptext(f, buf, 4);
}

void ia_gen_store(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                  enum gp_reg src, enum oz oz);

void gp_gen_store(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                  enum gp_reg src, enum oz oz);

void ia_gen_mov_mem_imm8(struct objfile *f,
                         enum gp_reg dest,
                         int32_t dest_disp,
                         int8_t imm) {
  check_y86x64(f);
  uint8_t b[11];
  b[0] = 0xC6;
  size_t count = x86_encode_reg_rm(b + 1, 0, map_x86_reg(dest), dest_disp);
  CHECK(count <= 9);
  b[1 + count] = (uint8_t)imm;
  apptext(f, b, 1 + count + 1);
}

void ia_gen_movzx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz);

void x64_gen_load64(struct objfile *f, enum x64_reg dest, enum x64_reg src_addr,
                    int32_t src_disp) {
  uint8_t b[11];
  b[0] = kREXW;
  b[1] = 0x8B;
  size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
  CHECK(count <= 9);
  apptext(f, b, count + 2);
}

void ia_gen_movsx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz) {
  check_y86x64(f);
  if (src_oz <= OZ_16) {
    uint8_t b[11];
    b[0] = 0x0F;
    b[1] = 0xBF ^ (src_oz == OZ_8);
    size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
    CHECK(count <= 9);
    apptext(f, b, count + 2);
  } else if (src_oz == OZ_32) {
    switch (objfile_arch(f)) {
    case TARGET_ARCH_Y86:
      ia_gen_movzx(f, dest, src_addr, src_disp, OZ_32);
      break;
    case TARGET_ARCH_X64: {
      uint8_t b[11];
      b[0] = kREXW;
      b[1] = 0x63;
      size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
      CHECK(count <= 9);
      apptext(f, b, count + 2);
    } break;
    default:
      UNREACHABLE();
    }
  } else {
    CHECK(objfile_arch(f) == TARGET_ARCH_X64);
    x64_gen_load64(f, map_x64_reg(dest), map_x64_reg(src_addr), src_disp);
  }
}

void gp_gen_movsx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz) {
  check_y86x64(f);
  ia_gen_movsx(f, dest, src_addr, src_disp, src_oz);
}

/* oz depicts the source operand -- the dest is always the full
register, which gets zero-extended. */
void ia_gen_movzx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz) {
  if (src_oz <= OZ_16) {
    uint8_t pref[2];
    pref[0] = 0x0F;
    pref[1] = 0xB6 + (src_oz == OZ_16);
    apptext(f, pref, 2);
  } else {
    ia_prefix(f, 0x8B, src_oz);
  }
  uint8_t b[9];
  size_t count = x86_encode_reg_rm(b, dest, src_addr, src_disp);
  CHECK(count <= 9);
  apptext(f, b, count);
}

void gp_gen_movzx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz) {
  check_y86x64(f);
  ia_gen_movzx(f, dest, src_addr, src_disp, src_oz);
}

void ia_gen_movzx8_reg8(struct objfile *f, enum gp_reg dest, enum x86_reg8 src) {
  uint8_t b[3];
  b[0] = 0x0F;
  b[1] = 0xB6;
  b[2] = mod_reg_rm(MOD11, dest, src);
  apptext(f, b, 3);
}

void ia_gen_lea(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                int32_t src_disp) {
  ia_prefix(f, 0x8D, ptr_oz(f));
  uint8_t b[9];
  size_t count = x86_encode_reg_rm(b, dest, src_addr, src_disp);
  CHECK(count <= 9);
  apptext(f, b, count);
}

void gp_gen_lea(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                int32_t src_disp) {
  check_y86x64(f);
  ia_gen_lea(f, dest, src_addr, src_disp);
}

/* Used for OS X 32-bit position-independent code.  Assigns X+srcdest to srcdest. */
size_t x86_gen_placeholder_lea32(struct objfile *f, enum x86_reg srcdest) {
  uint8_t b[10];
  b[0] = 0x8D;
  size_t reg_rm_reloc;
  size_t count = x86_encode_placeholder_reg_rm(b + 1, srcdest, &reg_rm_reloc);
  CHECK(count <= 9);
  /* The index into objfile_text() of the relocatable dword. */
  size_t ix = objfile_section_size(objfile_text(f)) + 1 + reg_rm_reloc;
  apptext(f, b, count + 1);
  return ix;
}

void ia_gen_store(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                  enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x89, oz);
  uint8_t b[9];
  size_t count = x86_encode_reg_rm(b, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  apptext(f, b, count);
}

void gp_gen_store(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                  enum gp_reg src, enum oz oz) {
  check_y86x64(f);
  ia_gen_store(f, dest_addr, dest_disp, src, oz);
}


void x64_gen_store64(struct objfile *f, enum x64_reg dest_addr, int32_t dest_disp,
                     enum x64_reg src) {
  /* TODO(): I think this needs to support upper registers. */
  CHECK(dest_addr <= X64_RDI && src <= X64_RDI);
  uint8_t b[11];
  b[0] = kREXW;
  b[1] = 0x89;
  size_t count = x86_encode_reg_rm(b + 2, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  apptext(f, b, count + 2);
}

void gen_function_intro(struct objfile *f, struct frame *h) {
  switch (h->arch) {
  case TARGET_ARCH_Y86:
    ia_gen_push(f, GP_BP);
    gp_gen_mov_reg(f, GP_BP, GP_SP);
    gen_placeholder_stack_adjustment(f, h, 1);
    break;
  case TARGET_ARCH_X64:
    ia_gen_push(f, GP_BP);
    gp_gen_mov_reg(f, GP_BP, GP_SP);
    gen_placeholder_stack_adjustment(f, h, 1);
    break;
  default:
    UNREACHABLE();
  }
}

void y86_push_address(struct objfile *f, struct frame *h, struct loc loc) {
  CHECK(h->arch == TARGET_ARCH_Y86);
  struct loc dest = frame_push_loc(h, ptr_size(h->arch));
  gen_mov_addressof(f, dest, loc);
}

int platform_ret4_hrp(struct checkstate *cs) {
  switch (cs->platform) {
  case TARGET_PLATFORM_WIN_32BIT:
    return 0;
  case TARGET_PLATFORM_LINUX_32BIT: /* fallthrough */
  case TARGET_PLATFORM_OSX_32BIT:
    return 1;
  case TARGET_PLATFORM_LINUX_64BIT: /* fallthrough */
  case TARGET_PLATFORM_OSX_64BIT:
    CRASH("We shouldn't be asking this question for x64.");
  default:
    UNREACHABLE();
  }
}

/* chase mark */
void gen_call_imm_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                       struct sti func_sti,
                       int hidden_return_param) {
  /* Dupes code with typetrav_call_func. */
  gen_placeholder_stack_adjustment(f, h, 0);
  /* y86/x64 */
  ia_gen_call(f, func_sti);
  switch (cs->arch) {
  case TARGET_ARCH_Y86: {
    if (hidden_return_param && platform_ret4_hrp(cs)) {
      /* TODO: We could do this more elegantly, but right now undo the
      callee's pop of esp. */
      x86_gen_add_esp_i32(f, -4);
    }
  } break;
  case TARGET_ARCH_X64:
    break;
  default:
    UNREACHABLE();
  }
  gen_placeholder_stack_adjustment(f, h, 1);
}

/* chase mark */
void gen_call_imm(struct checkstate *cs, struct objfile *f, struct frame *h,
                  struct immediate imm,
                  int hidden_return_param) {
  switch (imm.tag) {
  case IMMEDIATE_FUNC:
    gen_call_imm_func(cs, f, h, imm.u.func_sti, hidden_return_param);
    break;
  case IMMEDIATE_U64: /* fallthrough... */
  case IMMEDIATE_I64:
  case IMMEDIATE_U32:
  case IMMEDIATE_I32:
  case IMMEDIATE_U8:
  case IMMEDIATE_I8:
  case IMMEDIATE_VOID:
  default:
    UNREACHABLE();
  }
}

/* Put this right beneath where you save the stack offset. */
/* TODO() -- uh, this is complete nonsense for all callers of this?
On x86 things are passed in registers, so the arglist types matter. */
void adjust_frame_for_callsite_alignment(struct frame *h, uint32_t arglist_size) {
  /* y86/x64 - particularly for 32-bit OS X's 16-byte alignment. */
  int32_t unadjusted_callsite_offset
    = int32_sub(h->stack_offset, uint32_to_int32(arglist_size));

  uint32_t callsite_adjustment;
  switch (h->arch) {
  case TARGET_ARCH_Y86: {
    /* We want to lower the stack so that the callsite will be
    8-mod-16 (because we start counting below the ret-ptr and
    ebp-ptr). */
    /* We specifically _don't_ want overflow checking on this addition
    -- unadjusted_callsite_offset could easily be -4, or -8. */
    callsite_adjustment = (8 + (uint32_t)unadjusted_callsite_offset) % 16;
    CHECK(callsite_adjustment % Y86_DWORD_SIZE == 0);
  } break;
  case TARGET_ARCH_X64: {
    /* We want to lower the stack so that the callsite will be
    0-mod-16.  (We start counting below the ret-ptr and our gratuitous
    rbp-ptr, but they have size 8 + 8 = 16.) */
    callsite_adjustment = ((uint32_t)unadjusted_callsite_offset) % 16;
    CHECK(callsite_adjustment % X64_EIGHTBYTE_SIZE == 0);
  } break;
  default:
    UNREACHABLE();
  }
  frame_push_exact_amount(h, callsite_adjustment);
}

void typetrav_call_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                        struct def_instantiation *inst);

/* chase mark */
void gen_typetrav_onearg_call(struct checkstate *cs, struct objfile *f,
                              struct frame *h,
                              struct loc loc, struct def_instantiation *inst) {
  int32_t stack_offset = frame_save_offset(h);
  switch (cs->arch) {
  case TARGET_ARCH_Y86:
    /* pointer passed on stack */
    adjust_frame_for_callsite_alignment(h, Y86_DWORD_SIZE);
    y86_push_address(f, h, loc);
    break;
  case TARGET_ARCH_X64:
    /* pointer passed in register */
    adjust_frame_for_callsite_alignment(h, 0);
    x64_gen_load_register(f, x64_param_regs[0], loc);
    break;
  default:
    UNREACHABLE();
  }
  typetrav_call_func(cs, f, h, inst);
  frame_restore_offset(h, stack_offset);
}

/* chase mark */
void gen_typetrav_twoarg_call(struct checkstate *cs, struct objfile *f,
                              struct frame *h,
                              struct loc dest, struct loc src,
                              struct def_instantiation *inst) {
  int32_t stack_offset = frame_save_offset(h);
  switch (cs->arch) {
  case TARGET_ARCH_Y86:
    /* pointer passed on stack */
    adjust_frame_for_callsite_alignment(h, 2 * Y86_DWORD_SIZE);
    y86_push_address(f, h, src);
    y86_push_address(f, h, dest);
    break;
  case TARGET_ARCH_X64:
    /* pointer passed in registers */
    adjust_frame_for_callsite_alignment(h, 0);
    x64_gen_load_register(f, x64_param_regs[1], src);
    x64_gen_load_register(f, x64_param_regs[0], dest);
    break;
  default:
    UNREACHABLE();
  }
  typetrav_call_func(cs, f, h, inst);
  frame_restore_offset(h, stack_offset);
}

/* chase mark */
int gen_typetrav_name_direct(struct checkstate *cs, struct objfile *f, struct frame *h,
                             enum typetrav_func tf, struct loc dest, struct loc src,
                             struct typeexpr_traits *traits,
                             struct typeexpr_trait_instantiations *insts) {
  switch (tf) {
  case TYPETRAV_FUNC_DESTROY: {
    CHECK(traits->copyable != TYPEEXPR_TRAIT_TRIVIALLY_HAD);

    if (!insts->destroy_inst) {
      /* No destroy inst.  Therefore, we haven't destroyed the
      object. */
      return 0;
    }

    gen_typetrav_onearg_call(cs, f, h, dest, insts->destroy_inst);
    return 1;
  } break;
  case TYPETRAV_FUNC_COPY: {
    CHECK(traits->copyable != TYPEEXPR_TRAIT_LACKED);
    CHECK(traits->copyable != TYPEEXPR_TRAIT_TRIVIALLY_HAD);

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

    CHECK(traits->movable != TYPEEXPR_TRAIT_TRIVIALLY_HAD);

    if (!insts->move_inst) {
      /* No move inst.  Therefore, we haven't moved the object. */
      return 0;
    }

    gen_typetrav_twoarg_call(cs, f, h, dest, src, insts->move_inst);
    return 1;
  } break;
  case TYPETRAV_FUNC_DEFAULT_CONSTRUCT: {
    CHECK(traits->inittible != TYPEEXPR_TRAIT_LACKED);
    CHECK(traits->inittible != TYPEEXPR_TRAIT_TRIVIALLY_HAD);

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

/* chase mark */
struct loc make_enum_num_loc(struct objfile *f,
                             struct frame *h,
                             struct loc loc) {
  uint32_t tag_size = enum_tag_size(h->arch);
  CHECK(loc.size >= tag_size);
  /* All enums start with a tag, in [0, tag_size). */
  return gen_subobject_loc(f, h, loc, tag_size, 0);
}

/* chase mark */
struct loc make_enum_body_loc(struct objfile *f,
                              struct frame *h,
                              struct loc loc,
                              uint32_t body_size,
                              uint32_t body_alignment) {
  uint32_t tag_size = enum_tag_size(h->arch);
  uint32_t body_offset = uint32_max(tag_size, body_alignment);
  CHECK(loc.size >= uint32_add(body_offset, body_size));
  /* All enums start with a tag, in [0, tag_size). */
  return gen_subobject_loc(f, h, loc, body_size, body_offset);
}

void gen_typetrav_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                       enum typetrav_func tf, struct loc dest, int has_src,
                       struct loc src, struct ast_typeexpr *type);

/* chase mark */
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

    gp_gen_load_register(f, GP_A, enum_num_loc);

    size_t end_target = frame_add_target(h);
    size_t next_target = frame_add_target(h);

    STATIC_CHECK(FIRST_ENUM_TAG_NUMBER == 1);
    for (size_t tagnum = 0, e = size_add(1, rhs->u.enumspec.enumfields_count);
         tagnum < e; tagnum++) {
      ia_gen_cmp_imm(f, GP_A, size_to_int32(tagnum), ptr_oz(f));
      gen_placeholder_jcc(f, h, IA_JCC_NE, next_target);
      switch (tf) {
      case TYPETRAV_FUNC_DESTROY:
        /* Do nothing. */
        break;
      case TYPETRAV_FUNC_COPY: /* fallthrough */
      case TYPETRAV_FUNC_MOVE_OR_COPYDESTROY:
        gp_gen_store_register(f, dest_num_loc, GP_A);
        break;
      case TYPETRAV_FUNC_DEFAULT_CONSTRUCT:
        /* Unreachable because default construction is a trivial
        operation. */
        UNREACHABLE();
      default:
        UNREACHABLE();
      }

      if (tagnum != 0) {
        struct type_attrs field_attrs = gp_attrsof(&cs->nt, &rhs->u.enumspec.enumfields[tagnum - FIRST_ENUM_TAG_NUMBER].type);
        struct loc dest_body_loc = make_enum_body_loc(f, h, dest, field_attrs.size,
                                                      field_attrs.align);
        struct loc src_body_loc;
        if (has_src) {
          src_body_loc = make_enum_body_loc(f, h, src, field_attrs.size,
                                            field_attrs.align);
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

/* chase mark */
int try_gen_trivial_typetrav_func(struct checkstate *cs, struct objfile *f,
                                  enum typetrav_func tf,
                                  struct loc dest, struct loc src,
                                  struct ast_typeexpr *type) {
  struct typeexpr_traits traits;
  int success = check_typeexpr_traits(cs, type, NULL, &traits);
  CHECK(success);
  switch (tf) {
  case TYPETRAV_FUNC_DESTROY: {
    if (traits.copyable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* Destroying a trivial type: Do nothing. */
      return 1;
    }
  } break;
  case TYPETRAV_FUNC_COPY: {
    if (traits.copyable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* Copying a trivial type:  Copy it.. trivially. */
      gen_mov(f, dest, src);
      return 1;
    }
  } break;
  case TYPETRAV_FUNC_MOVE_OR_COPYDESTROY: {
    /* Moving trivially is also trivial. */
    if (traits.movable == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      gen_mov(f, dest, src);
      return 1;
    }
  } break;
  case TYPETRAV_FUNC_DEFAULT_CONSTRUCT: {
    if (traits.inittible == TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      /* A previous comment (in gen_typetrav_name_direct) said we
      don't need to zero the variable but I don't get what that's
      about -- AFAICT we do need to zero a variable to
      default-construct it. */
      gen_bzero(f, dest);
      return 1;
    }
  } break;
  }
  return 0;
}

/* chase mark */
void really_gen_typetrav_behavior(struct checkstate *cs, struct objfile *f,
                                  struct frame *h, enum typetrav_func tf,
                                  struct loc dest, int has_src,
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
      struct loc dest_field_loc = gen_field_loc(cs, f, h, dest, type,
                                                type->u.structe.fields[i].name.value);
      struct loc src_field_loc;
      if (has_src) {
        src_field_loc = gen_field_loc(cs, f, h, src, type,
                                      type->u.structe.fields[i].name.value);
      } else {
        src_field_loc.tag = (enum loc_tag)-1;
      }

      gen_typetrav_func(cs, f, h, tf, dest_field_loc, has_src, src_field_loc,
                        &type->u.structe.fields[i].type);
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
    uint32_t arraytype_count = unsafe_numeric_literal_u32(&type->u.arraytype.number);
    for (uint32_t i = 0; i < arraytype_count; i++) {
      int32_t saved_offset = frame_save_offset(h);
      struct loc dest_element_loc = gen_array_element_loc(cs, f, h, dest,
                                                          type->u.arraytype.param, i);
      struct loc src_element_loc;
      if (has_src) {
        src_element_loc = gen_array_element_loc(cs, f, h, src,
                                                type->u.arraytype.param, i);
      } else {
        src_element_loc.tag = (enum loc_tag)-1;
      }

      gen_typetrav_func(cs, f, h, tf, dest_element_loc, has_src, src_element_loc,
                        type->u.arraytype.param);

      frame_restore_offset(h, saved_offset);
    }
  } break;
  default:
    UNREACHABLE();
  }
}

void make_typetrav_sti_name(struct identmap *im,
                            enum typetrav_func tf,
                            struct ast_typeexpr *type,
                            struct databuf *out) {
  struct databuf b;
  databuf_init(&b);
  static const char *const names[] = {
    [TYPETRAV_FUNC_DESTROY] = "destroy:",
    [TYPETRAV_FUNC_COPY] = "copy:",
    [TYPETRAV_FUNC_MOVE_OR_COPYDESTROY] = "mocd:",
    [TYPETRAV_FUNC_DEFAULT_CONSTRUCT] = "init:",
  };
  databuf_append_c_str(&b, names[tf]);
  sprint_typeexpr(&b, im, type);
  *out = b;
}

struct sti lookup_or_make_typetrav_sti(
    struct objfile *f,
    struct checkstate *cs, enum typetrav_func tf,
    struct ast_typeexpr *type) {
  ident_value id;
  {
    struct databuf name;
    make_typetrav_sti_name(cs->im, tf, type, &name);
    id = identmap_intern(&cs->typetrav_values, name.buf, name.count);
    databuf_destroy(&name);
  }
  CHECK(id <= cs->typetrav_symbol_infos_count);
  if (id == cs->typetrav_symbol_infos_count) {
    struct databuf namebuf;
    databuf_init(&namebuf);
    switch (tf) {
    case TYPETRAV_FUNC_DESTROY:
      databuf_append_c_str(&namebuf, "$destroy[");
      break;
    case TYPETRAV_FUNC_COPY:
      databuf_append_c_str(&namebuf, "$copy[");
      break;
    case TYPETRAV_FUNC_MOVE_OR_COPYDESTROY:
      databuf_append_c_str(&namebuf, "$moveor[");
      break;
    case TYPETRAV_FUNC_DEFAULT_CONSTRUCT:
      databuf_append_c_str(&namebuf, "$init[");
      break;
    }
    sprint_typeexpr(&namebuf, cs->im, type);
    databuf_append_c_str(&namebuf, "]");
    void *gen_name;
    size_t gen_name_count;
    generate_kit_name(cs, namebuf.buf, namebuf.count,
                      0, &gen_name, &gen_name_count);
    databuf_destroy(&namebuf);

    struct sti symbol_table_index
      = objfile_add_local_symbol(f, identmap_intern(cs->im, gen_name, gen_name_count),
                                 0 /* We'll overwrite the value later. */,
                                 SECTION_TEXT,
                                 IS_STATIC_NO);
    free(gen_name);

    struct typetrav_symbol_info *info = malloc(sizeof(*info));
    CHECK(info);
    info->symbol_table_index = symbol_table_index;
    info->func = tf;
    ast_typeexpr_init_copy(&info->type, type);
    SLICE_PUSH(cs->typetrav_symbol_infos, cs->typetrav_symbol_infos_count,
               cs->typetrav_symbol_infos_limit, info);
  }
  return cs->typetrav_symbol_infos[id]->symbol_table_index;
}

/* chase mark */
void gen_typetrav_func(struct checkstate *cs, struct objfile *f, struct frame *h,
                       enum typetrav_func tf, struct loc dest, int has_src,
                       struct loc src, struct ast_typeexpr *type) {
  if (try_gen_trivial_typetrav_func(cs, f, tf, dest, src, type)) {
    return;
  }
  /* TODO: Go deeper on names to avoid needless function layers. */
  struct sti sti = lookup_or_make_typetrav_sti(f, cs, tf, type);

  int32_t saved_offset = frame_save_offset(h);
  switch (cs->arch) {
  case TARGET_ARCH_Y86: {
    if (has_src) {
      /* X86 - pointer size */
      adjust_frame_for_callsite_alignment(h, 2 * Y86_DWORD_SIZE);
      y86_push_address(f, h, src);
    } else {
      /* X86 - pointer size */
      adjust_frame_for_callsite_alignment(h, Y86_DWORD_SIZE);
    }
    y86_push_address(f, h, dest);
  } break;
  case TARGET_ARCH_X64: {
    /* TODO(): Generally the "callsite alignment" logic will need to
    be tweaked for x64, in the more general case.  We pass parameters
    in registers here. */
    adjust_frame_for_callsite_alignment(h, 0);
    if (has_src) {
      x64_gen_load_addressof(f, X64_RSI, src);
    }
    x64_gen_load_addressof(f, X64_RDI, dest);
  } break;
  }
  gen_call_imm_func(cs, f, h, sti, 0);
  frame_restore_offset(h, saved_offset);
}

/* chase mark */
void gen_destroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                 struct loc loc, struct ast_typeexpr *type) {
  struct loc ignore;
  ignore.tag = (enum loc_tag)-1;
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_DESTROY, loc, 0, ignore, type);
}

/* chase mark */
void gen_copy(struct checkstate *cs, struct objfile *f, struct frame *h,
              struct loc dest, struct loc src, struct ast_typeexpr *type) {
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_COPY, dest, 1, src, type);
}
/* chase mark */
void gen_move_or_copydestroy(struct checkstate *cs, struct objfile *f, struct frame *h,
                             struct loc dest, struct loc src,
                             struct ast_typeexpr *type) {
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_MOVE_OR_COPYDESTROY, dest, 1, src, type);
}
/* chase mark */
void gen_default_construct(struct checkstate *cs, struct objfile *f, struct frame *h,
                           struct loc loc, struct ast_typeexpr *type) {
  struct loc ignore;
  ignore.tag = (enum loc_tag)-1;
  gen_typetrav_func(cs, f, h, TYPETRAV_FUNC_DEFAULT_CONSTRUCT, loc, 0, ignore, type);
}

void x86_gen_returnloc_funcreturn_convention(struct objfile *f,
                                             int hidden_return_param,
                                             struct loc return_loc) {
  /* return_loc is always in the stack frame (and padded to
  Y86_DWORD_SIZE) or via a hidden return param. */
  if (hidden_return_param) {
    CHECK(return_loc.tag == LOC_EBP_INDIRECT);
    ia_gen_movzx(f, GP_A, GP_BP, return_loc.u.ebp_indirect, OZ_32);
  } else if (return_loc.size == 0) {
    ia_gen_xor(f, GP_A, GP_A, OZ_32);
  } else if (return_loc.size <= Y86_DWORD_SIZE) {
    CHECK(return_loc.tag == LOC_EBP_OFFSET);
    CHECK(return_loc.padded_size == Y86_DWORD_SIZE);
    ia_gen_movzx(f, GP_A, GP_BP, return_loc.u.ebp_offset, OZ_32);
  } else {
    /* TODO: This should not even be theoretically reachable on (LINUX) linux32. */
    CHECK(return_loc.size == 2 * Y86_DWORD_SIZE);
    CHECK(return_loc.tag == LOC_EBP_OFFSET);
    CHECK(return_loc.padded_size == 2 * Y86_DWORD_SIZE);
    ia_gen_movzx(f, GP_A, GP_BP, return_loc.u.ebp_offset, OZ_32);
    ia_gen_movzx(f, GP_D, GP_BP,
                 int32_add(return_loc.u.ebp_offset, Y86_DWORD_SIZE), OZ_32);
  }
}

void x64_gen_returnloc_funcreturn_convention(struct objfile *f,
                                             int hidden_return_param,
                                             struct loc return_loc) {
  (void)f, (void)hidden_return_param, (void)return_loc;
  TODO_IMPLEMENT;
}

void gen_function_exit(struct checkstate *cs, struct objfile *f, struct frame *h) {
  if (h->return_target_valid) {
    frame_define_target(h, h->return_target_number,
                        objfile_section_size(objfile_text(f)));
  }

  CHECK(frame_arg_count(h) == h->vardata_count);
  for (size_t i = 0, e = frame_arg_count(h); i < e; i++) {
    struct vardata *vd = &h->vardata[size_sub(h->vardata_count, 1)];
    if (vd->destroy_when_unwound) {
      gen_destroy(cs, f, h, vd->loc, vd->concrete_type);
    }
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  }

  int hidden_return_param = frame_hidden_return_param(h);
  switch (h->arch) {
  case TARGET_ARCH_Y86: {
    x86_gen_returnloc_funcreturn_convention(f, hidden_return_param,
                                            h->return_loc);

    gp_gen_mov_reg(f, GP_SP, GP_BP);
    ia_gen_pop(f, GP_BP);
    if (hidden_return_param && platform_ret4_hrp(cs)) {
      x86_gen_retn(f, 4);
    } else {
      ia_gen_ret(f);
    }
  } break;
  case TARGET_ARCH_X64: {
    x64_gen_returnloc_funcreturn_convention(f, hidden_return_param,
                                            h->return_loc);
    gp_gen_mov_reg(f, GP_SP, GP_BP);
    ia_gen_pop(f, GP_BP);
    ia_gen_ret(f);
  } break;
  default:
    UNREACHABLE();
  }

  if (h->crash_target_exists) {
    frame_define_target(h, h->crash_target_number,
                        objfile_section_size(objfile_text(f)));
    switch (h->arch) {
    case TARGET_ARCH_Y86: {
      x86_gen_int_3(f);
    } break;
    case TARGET_ARCH_X64:
      TODO_IMPLEMENT;
      break;
    default:
      UNREACHABLE();
    }
  }
}

enum x86_reg x86_choose_altreg(enum x86_reg used) {
  if (used == X86_EAX) {
    return X86_ECX;
  } else {
    return X86_EAX;
  }
}

enum gp_reg gp_choose_altreg(enum gp_reg used) {
  if (used == GP_A) {
    return GP_C;
  } else {
    return GP_A;
  }
}

enum x86_reg x86_choose_register_2(enum x86_reg used1, enum x86_reg used2) {
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

enum gp_reg gp_choose_register_2(enum gp_reg used1, enum gp_reg used2) {
  if (used1 == GP_A || used2 == GP_A) {
    if (used1 == GP_C || used2 == GP_C) {
      return GP_D;
    } else {
      return GP_C;
    }
  } else {
    return GP_A;
  }
}

void x86_gen_load_addressof(struct objfile *f, enum x86_reg dest, struct loc loc) {
  switch (loc.tag) {
  case LOC_EBP_OFFSET: {
    ia_gen_lea(f, unmap_x86_reg(dest), GP_BP, loc.u.ebp_offset);
  } break;
  case LOC_GLOBAL: {
    x86_gen_mov_reg_stiptr(f, dest, loc.u.global_sti);
  } break;
  case LOC_EBP_INDIRECT: {
    ia_gen_movzx(f, unmap_x86_reg(dest), GP_BP, loc.u.ebp_indirect, OZ_32);
  } break;
  default:
    UNREACHABLE();
  }
}

void x64_gen_load_addressof(struct objfile *f, enum x64_reg dest, struct loc loc) {
  (void)f, (void)dest, (void)loc;
  TODO_IMPLEMENT;
}

void gp_gen_load_addressof(struct objfile *f, enum gp_reg dest, struct loc loc) {
  switch (objfile_arch(f)) {
  case TARGET_ARCH_Y86:
    x86_gen_load_addressof(f, map_x86_reg(dest), loc);
    break;
  case TARGET_ARCH_X64:
    TODO_IMPLEMENT;
    break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
void gen_mov_addressof(struct objfile *f, struct loc dest, struct loc loc) {
  CHECK(dest.size == ptr_size(objfile_arch(f)));
  gp_gen_load_addressof(f, GP_A, loc);
  gp_gen_store_register(f, dest, GP_A);
}

void gp_gen_memmem_mov(struct objfile *f,
                       enum gp_reg dest_reg,
                       int32_t dest_disp,
                       enum gp_reg src_reg,
                       int32_t src_disp,
                       uint32_t upadded_size) {
  int32_t padded_size = uint32_to_int32(upadded_size);
  enum gp_reg reg = gp_choose_register_2(dest_reg, src_reg);
  int32_t n = 0;
  while (n < padded_size) {
    if (padded_size - n >= 4) {
      gp_gen_movzx(f, reg, src_reg, int32_add(n, src_disp), OZ_32);
      gp_gen_store(f, dest_reg, int32_add(n, dest_disp), reg, OZ_32);
      n += 4;
    } else {
      gp_gen_movzx(f, reg, src_reg, int32_add(n, src_disp), OZ_8);
      gp_gen_store(f, dest_reg, int32_add(n, dest_disp), reg, OZ_8);
      n += 1;
    }
  }
}

void put_ptr_in_reg(struct objfile *f, struct loc loc, enum gp_reg free_reg,
                    enum gp_reg *reg_out, int32_t *disp_out) {
  switch (loc.tag) {
  case LOC_EBP_OFFSET: {
    *reg_out = GP_BP;
    *disp_out = loc.u.ebp_offset;
  } break;
  case LOC_GLOBAL: {
    gp_gen_mov_reg_stiptr(f, free_reg, loc.u.global_sti);
    *reg_out = free_reg;
    *disp_out = 0;
  } break;
  case LOC_EBP_INDIRECT: {
    gp_gen_movzx(f, free_reg, GP_BP, loc.u.ebp_indirect, ptr_oz(f));
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
/* chase mark */
void gen_mov(struct objfile *f, struct loc dest, struct loc src) {
  CHECK(dest.size == src.size);
  if (loc_equal(dest, src)) {
    return;
  }

  CHECK(dest.tag != LOC_GLOBAL);

  enum gp_reg dest_reg;
  int32_t dest_disp;
  put_ptr_in_reg(f, dest, GP_A, &dest_reg, &dest_disp);
  enum gp_reg src_reg;
  int32_t src_disp;
  put_ptr_in_reg(f, src, GP_D, &src_reg, &src_disp);

  uint32_t padded_size = dest.padded_size < src.padded_size ? dest.padded_size : src.padded_size;
  CHECK(padded_size >= src.size);
  gp_gen_memmem_mov(f, dest_reg, dest_disp, src_reg, src_disp, padded_size);
}

void gp_gen_mem_bzero(struct objfile *f, enum gp_reg reg, int32_t disp,
                      uint32_t upadded_size) {
  enum gp_reg zreg = gp_choose_altreg(reg);
  gp_gen_mov_reg_imm32(f, zreg, 0);

  int32_t padded_size = uint32_to_int32(upadded_size);
  int32_t n = 0;
  while (n < padded_size) {
    if (padded_size - n >= 4) {
      gp_gen_store(f, reg, int32_add(n, disp), zreg, OZ_32);
      n += 4;
    } else {
      gp_gen_store(f, reg, int32_add(n, disp), zreg, OZ_8);
      n += 1;
    }
  }
}

/* chase mark */
void gen_bzero(struct objfile *f, struct loc dest) {
  enum gp_reg reg;
  int32_t disp;
  put_ptr_in_reg(f, dest, GP_A, &reg, &disp);

  gp_gen_mem_bzero(f, reg, disp, dest.padded_size);
}

void gp_gen_store_register(struct objfile *f, struct loc dest, enum gp_reg reg) {
  enum gp_reg dest_addr;
  int32_t dest_disp;
  put_ptr_in_reg(f, dest, gp_choose_altreg(reg), &dest_addr, &dest_disp);

  switch (dest.size) {
  case 8:
    TODO_IMPLEMENT;
    break;
  case 4:
    gp_gen_store(f, dest_addr, dest_disp, reg, OZ_32);
    break;
  case 2:
    gp_gen_store(f, dest_addr, dest_disp, reg, OZ_16);
    break;
  case 1:
    gp_gen_store(f, dest_addr, dest_disp, reg, OZ_8);
    break;
  default:
    CRASH("not implemented or unreachable.");
  }
}

void x64_gen_store_register(struct objfile *f, struct loc dest,
                            enum x64_reg reg, enum x64_reg spare) {
  /* We actually do have to implement this, because reg can be a
  calling convention register like r8 or r9, not named by gp_reg. */
  (void)f, (void)dest, (void)reg, (void)spare;
  TODO_IMPLEMENT;
}

void gp_gen_load_register(struct objfile *f, enum gp_reg reg, struct loc src) {
  enum gp_reg src_addr;
  int32_t src_disp;
  put_ptr_in_reg(f, src, reg, &src_addr, &src_disp);

  switch (src.size) {
  case 8:
    gp_gen_movzx(f, reg, src_addr, src_disp, OZ_64);
    break;
  case 4:
    gp_gen_movzx(f, reg, src_addr, src_disp, OZ_32);
    break;
  case 2:
    gp_gen_movzx(f, reg, src_addr, src_disp, OZ_16);
    break;
  case 1:
    gp_gen_movzx(f, reg, src_addr, src_disp, OZ_8);
    break;
  default:
    CRASH("not implemented or unreachable.");
  }
}

void x64_gen_load_register(struct objfile *f, enum x64_reg reg, struct loc src) {
  (void)f, (void)reg, (void)src;
  TODO_IMPLEMENT;
}


void x86_gen_store_biregister(struct objfile *f, struct loc dest,
                              enum x86_reg lo, enum x86_reg hi) {
  CHECK(Y86_DWORD_SIZE < dest.size && dest.size <= 2 * Y86_DWORD_SIZE);
  CHECK(dest.padded_size == 2 * Y86_DWORD_SIZE);
  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    ia_gen_store(f, GP_BP, dest.u.ebp_offset, unmap_x86_reg(lo), OZ_32);
    ia_gen_store(f, GP_BP, int32_add(dest.u.ebp_offset, Y86_DWORD_SIZE),
                 unmap_x86_reg(hi), OZ_32);
    break;
  case LOC_GLOBAL: {
    CRASH("Writing to globals is impossible.");
  } break;
  case LOC_EBP_INDIRECT: {
    enum x86_reg altreg = x86_choose_register_2(lo, hi);
    gp_gen_movzx(f, unmap_x86_reg(altreg), GP_BP, dest.u.ebp_indirect, OZ_32);
    ia_gen_store(f, unmap_x86_reg(altreg), 0, unmap_x86_reg(lo), OZ_32);
    ia_gen_store(f, unmap_x86_reg(altreg), Y86_DWORD_SIZE, unmap_x86_reg(hi), OZ_32);
  } break;
  default:
    UNREACHABLE();
  }
}

void x64_gen_store_biregister(struct objfile *f, struct loc dest,
                              enum x64_reg lo, enum x64_reg hi, enum x64_reg spare) {
  CHECK(X64_EIGHTBYTE_SIZE < dest.size && dest.size <= 2 * X64_EIGHTBYTE_SIZE);
  CHECK(dest.padded_size == 2 * X64_EIGHTBYTE_SIZE);
  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    x64_gen_store64(f, X64_RBP, dest.u.ebp_offset, lo);
    x64_gen_store64(f, X64_RBP, int32_add(dest.u.ebp_offset, X64_EIGHTBYTE_SIZE), hi);
    break;
  case LOC_GLOBAL: {
    CRASH("Writing to globals is impossible.");
  } break;
  case LOC_EBP_INDIRECT: {
    x64_gen_load64(f, spare, X64_RBP, dest.u.ebp_indirect);
    x64_gen_store64(f, spare, 0, lo);
    x64_gen_store64(f, spare, X64_EIGHTBYTE_SIZE, hi);
  } break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
void gen_mov_mem_imm(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                     enum gp_reg aux,
                     struct immediate src) {
  switch (src.tag) {
  case IMMEDIATE_FUNC: {
    /* Doesn't just use an immediate because OS X 32-bit won't work
    that way. */
    gp_gen_mov_reg_stiptr(f, aux, src.u.func_sti);
    gp_gen_store(f, dest_addr, dest_disp, aux, ptr_oz(f));
  } break;
  case IMMEDIATE_U64: {
    TODO_IMPLEMENT;
  } break;
  case IMMEDIATE_I64: {
    TODO_IMPLEMENT;
  } break;
  case IMMEDIATE_U32: {
    char buf[4];
    write_le_u32(buf, src.u.u32);
    ia_help_gen_mov_mem_imm32(f, dest_addr, dest_disp, buf);
  } break;
  case IMMEDIATE_I32: {
    char buf[4];
    write_le_i32(buf, src.u.i32);
    ia_help_gen_mov_mem_imm32(f, dest_addr, dest_disp, buf);
  } break;
  case IMMEDIATE_U8: {
    ia_gen_mov_mem_imm8(f, dest_addr, dest_disp, (int8_t)src.u.u8);
  } break;
  case IMMEDIATE_I8: {
    ia_gen_mov_mem_imm8(f, dest_addr, dest_disp, src.u.i8);
  } break;
  case IMMEDIATE_VOID: {
    /* Do nothing. */
  } break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
void gen_mov_immediate(struct objfile *f, struct loc dest, struct immediate src) {
  CHECK(dest.size == immediate_size(objfile_arch(f), src));

  switch (dest.tag) {
  case LOC_EBP_OFFSET:
    gen_mov_mem_imm(f, GP_BP, dest.u.ebp_offset, GP_D, src);
    break;
  case LOC_EBP_INDIRECT:
    gp_gen_movzx(f, GP_A, GP_BP, dest.u.ebp_indirect, ptr_oz(f));
    gen_mov_mem_imm(f, GP_A, 0, GP_D, src);
    break;
  case LOC_GLOBAL:
    CRASH("Global mutation should be impossible.");
  default:
    UNREACHABLE();
  }
}


void ia_gen_call(struct objfile *f, struct sti func_sti) {
  uint8_t b = 0xE8;
  apptext(f, &b, 1);
  /* TODO(): x64 needs rel32 as the relocation type too, right? */
  objfile_section_append_rel32(objfile_text(f), func_sti);
}

void x86_gen_indirect_call_reg(struct objfile *f, enum x86_reg reg) {
  uint8_t b[2];
  b[0] = 0xFF;
  b[1] = mod_reg_rm(MOD11, 2, reg);
  apptext(f, b, 2);
}

void x64_gen_indirect_call_reg(struct objfile *f, enum x64_reg reg) {
  (void)f, (void)reg;
  TODO_IMPLEMENT;
}


enum expr_return_free_tag {
  EXPR_RETURN_FREE_LOC,
  EXPR_RETURN_FREE_IMM,
  EXPR_RETURN_FREE_PRIMITIVE_OP,
};

struct expr_return_free {
  enum expr_return_free_tag tag;
  union {
    struct loc loc;
    struct immediate imm;
    struct primitive_op primitive_op;
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

/* chase mark */
struct temp_return temp_none(void) {
  struct temp_return ret;
  ret.exists = 0;
  return ret;
}

/* chase mark */
struct temp_return temp_exists_trivial(struct loc loc,
                                       int whole_thing) {
  (void)loc, (void)whole_thing;
  return temp_none();
}

/* chase mark */
struct temp_return temp_immediate(void) {
  return temp_none();
}

/* chase mark */
struct temp_return temp_primitive_op(void) {
  return temp_none();
}

/* chase mark */
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

/* chase mark */
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

/* chase mark */
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

/* chase mark */
void er_set_tr(struct expr_return *er, struct temp_return tr) {
  CHECK(!er->has_tr);
  er->has_tr = 1;
  er->tr_ = tr;
}

/* chase mark */
struct temp_return *er_tr(struct expr_return *er) {
  CHECK(er->has_tr);
  return &er->tr_;
}

/* chase mark */
void gen_destroy_temp(struct checkstate *cs, struct objfile *f, struct frame *h,
                      struct temp_return tr) {
  if (tr.exists) {
    gen_destroy(cs, f, h, tr.loc, tr.temporary_type);
  }
}

/* chase mark */
void move_or_copy_temporary_into_loc(struct checkstate *cs, struct objfile *f,
                                     struct frame *h, struct loc dest, struct loc src,
                                     struct ast_typeexpr *type, struct temp_return tr) {
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
/* chase mark */
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

/* chase mark */
void wipe_temporaries(struct checkstate *cs, struct objfile *f, struct frame *h,
                      struct expr_return *src, struct ast_typeexpr *value_type,
                      struct loc *dest_out) {
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
    case EXPR_RETURN_FREE_PRIMITIVE_OP:
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

/* chase mark */
struct expr_return open_expr_return(void) {
  struct expr_return ret;
  ret.tag = EXPR_RETURN_OPEN;
  ret.u.open.has_loc = 0;
  ret.has_tr = 0;
  return ret;
}

/* chase mark */
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
  /* y86/x64 */
  ia_gen_call(f, di_symbol_table_index(inst));
  gen_placeholder_stack_adjustment(f, h, 1);
}


int gen_expr(struct checkstate *cs, struct objfile *f,
             struct frame *h, struct ast_expr *a,
             struct expr_return *ret);

void gen_cmp_behavior(struct objfile *f,
                      int32_t off0, int32_t off1,
                      enum ia_setcc setcc_code,
                      enum oz oz) {
  gp_gen_movzx(f, GP_D, GP_BP, off0, oz);
  gp_gen_movzx(f, GP_C, GP_BP, off1, oz);
  ia_gen_cmp(f, GP_D, GP_C, oz);
  ia_gen_setcc_b8(f, X86_AL, setcc_code);
  ia_gen_movzx8_reg8(f, GP_A, X86_AL);
}

/* TODO(): Check callers of this for use on pointers, where cmp64 might be what we want. */
/* TODO(): Replace with gen_cmp_behavior usage. */
void gen_cmp32_behavior(struct objfile *f,
                        int32_t off0, int32_t off1,
                        enum ia_setcc setcc_code) {
  gp_gen_movzx(f, GP_D, GP_BP, off0, OZ_32);
  gp_gen_movzx(f, GP_C, GP_BP, off1, OZ_32);
  ia_gen_cmp(f, GP_D, GP_C, OZ_32);
  ia_gen_setcc_b8(f, X86_AL, setcc_code);
  ia_gen_movzx8_reg8(f, GP_A, X86_AL);
}

void gen_cmp16_behavior(struct objfile *f,
                        int32_t off0, int32_t off1,
                        enum ia_setcc setcc_code) {
  gp_gen_movzx(f, GP_D, GP_BP, off0, OZ_16);
  gp_gen_movzx(f, GP_C, GP_BP, off1, OZ_16);
  ia_gen_cmp(f, GP_D, GP_C, OZ_16);
  ia_gen_setcc_b8(f, X86_AL, setcc_code);
  ia_gen_movzx8_reg8(f, GP_A, X86_AL);
}

void gen_cmp8_behavior(struct objfile *f,
                       int32_t off0, int32_t off1,
                       enum ia_setcc setcc_code) {
  gp_gen_movzx(f, GP_D, GP_BP, off0, OZ_8);
  gp_gen_movzx(f, GP_C, GP_BP, off1, OZ_8);
  ia_gen_cmp(f, GP_D, GP_C, OZ_8);
  ia_gen_setcc_b8(f, X86_AL, setcc_code);
  ia_gen_movzx8_reg8(f, GP_A, X86_AL);
}

/* chase mark */
void gen_enumconstruct_behavior(struct checkstate *cs,
                                struct objfile *f,
                                struct frame *h,
                                size_t enumconstruct_number,
                                struct ast_typeexpr *arg0_type,
                                struct loc return_loc,
                                struct loc arg_loc) {
  CHECK(arg0_type);
  /* We have to actually figure out the calling convention and
  arg/return locations and sizes for this op. */
  int32_t saved_stack_offset = frame_save_offset(h);

  struct type_attrs arg_attrs = gp_attrsof(&cs->nt, arg0_type);

  struct loc return_enum_num_loc = make_enum_num_loc(f, h, return_loc);
  struct loc return_enum_body_loc
    = make_enum_body_loc(f, h, return_loc, arg_attrs.size, arg_attrs.align);

  int32_t enum_num_i32
    = int32_add(FIRST_ENUM_TAG_NUMBER, size_to_int32(enumconstruct_number));
  gp_gen_mov_reg_imm32(f, GP_A, enum_num_i32);
  gp_gen_store_register(f, return_enum_num_loc, GP_A);

  gen_move_or_copydestroy(cs, f, h, return_enum_body_loc, arg_loc, arg0_type);

  frame_restore_offset(h, saved_stack_offset);
}

void gen_movzx_ac(struct objfile *f, int32_t off0, int32_t off1, enum oz oz) {
  gp_gen_movzx(f, GP_A, GP_BP, off0, oz);
  gp_gen_movzx(f, GP_C, GP_BP, off1, oz);
}

void gen_add_primop(struct objfile *f, struct frame *h, int32_t off0, int32_t off1,
                    enum oz oz, enum ia_jcc crash_case) {
  gen_movzx_ac(f, off0, off1, oz);
  ia_gen_add(f, GP_A, GP_C, oz);
  if (0 < (int)crash_case) {
    gen_crash_jcc(f, h, crash_case);
  }
}

void gen_sub_primop(struct objfile *f, struct frame *h, int32_t off0, int32_t off1,
                    enum oz oz, enum ia_jcc crash_case) {
  gen_movzx_ac(f, off0, off1, oz);
  ia_gen_sub(f, GP_A, GP_C, oz);
  if (0 < (int)crash_case) {
    gen_crash_jcc(f, h, crash_case);
  }
}

/* chase mark */
void gen_very_primitive_op_behavior(struct checkstate *cs,
                                    struct objfile *f,
                                    struct frame *h,
                                    struct primitive_op prim_op,

                                    /* is NULL if op has arity 0 */
                                    struct ast_typeexpr *arg0_type_or_null,
                                    int32_t callsite_base_offset,
                                    struct funcall_arglist_info *arglist_info) {
  int32_t off0 = INT32_MIN;
  int32_t off1 = INT32_MIN;
  if (arglist_info->args_count > 0) {
    off0 = int32_add(callsite_base_offset, arglist_info->arg_infos[0].relative_disp);
  }
  if (arglist_info->args_count > 1) {
    CHECK(arglist_info->args_count == 2);
    off1 = int32_add(callsite_base_offset, arglist_info->arg_infos[1].relative_disp);
  }

  /* So, what's the game here?  On y86, put the return value in EAX,
  or EAX:EDX.  On x64, put the return value in RAX, or RAX:RDX. */

  switch (prim_op.tag) {
  case PRIMITIVE_OP_ENUMCONSTRUCT: {
    UNREACHABLE();
  } break;
  case PRIMITIVE_OP_INIT: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(&cs->cm, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = gp_sizeof(&cs->nt, target);
    gen_default_construct(cs, f, h, ebp_indirect_loc(size, size, off0),
                          target);
  } break;
  case PRIMITIVE_OP_COPY: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(&cs->cm, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = gp_sizeof(&cs->nt, target);
    gen_copy(cs, f, h, ebp_indirect_loc(size, size, off0),
             ebp_indirect_loc(size, size, off1),
             target);
  } break;
  case PRIMITIVE_OP_MOVE: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(&cs->cm, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = gp_sizeof(&cs->nt, target);
    gen_move_or_copydestroy(cs, f, h, ebp_indirect_loc(size, size, off0),
                            ebp_indirect_loc(size, size, off1),
                            target);
  } break;
  case PRIMITIVE_OP_DESTROY: {
    struct ast_typeexpr *target;
    int success = view_ptr_target(&cs->cm, arg0_type_or_null, &target);
    CHECK(success);
    uint32_t size = gp_sizeof(&cs->nt, target);
    gen_destroy(cs, f, h, ebp_indirect_loc(size, size, off0),
                target);
  } break;

  case PRIMITIVE_OP_CONVERT_U8_TO_I8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_8);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_U8_TO_U8: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_I16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U8_TO_I32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_8);
  } break;

  case PRIMITIVE_OP_CONVERT_I8_TO_I8: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_I16: /* fallthrough */
    /* I haven't thought hard about how converting to osize should
    work, but I think sign extending and not failing is the right
    thing.  My opinion might change if we add a signed osize type, in
    which u32 -> osize -> sosize -> i32 might be the unchecking
    conversion (but isn't that gross). */
  case PRIMITIVE_OP_CONVERT_I8_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_I32: {
    gp_gen_movsx(f, GP_A, GP_BP, off0, OZ_8);
  } break;
  case PRIMITIVE_OP_CONVERT_I8_TO_U8: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I8_TO_U32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_8);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_S);
  } break;

  case PRIMITIVE_OP_CONVERT_U16_TO_U8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_16);
    ia_gen_cmp_imm(f, GP_A, 0xFF, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_I8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_16);
    ia_gen_cmp_imm(f, GP_A, 0x7F, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_I16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_16);
    ia_gen_cmp_imm(f, GP_A, 0x7FFF, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U16_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U16_TO_I32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_16);
  } break;

  case PRIMITIVE_OP_CONVERT_I16_TO_U8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_16);
    ia_gen_cmp_imm(f, GP_A, 0xFF, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_I8: {
    gp_gen_movsx(f, GP_A, GP_BP, off0, OZ_16);
    ia_gen_cmp_imm(f, GP_A, 0x7F, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_G);
    ia_gen_cmp_imm(f, GP_A, -0x80, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I16_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I16_TO_U32: {
    gp_gen_movsx(f, GP_A, GP_BP, off0, OZ_16);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_I16_TO_I16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I16_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I16_TO_I32: {
    gp_gen_movsx(f, GP_A, GP_BP, off0, OZ_16);
  } break;

  case PRIMITIVE_OP_CONVERT_SIZE_TO_U8: /* fallthrough */
    /* I think (without having thought hard) that converting _from_
    osize should fail if data is _lost_ but not if the osize variable,
    intepreted with the same signedness, has the same value. */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_U8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0xFF, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_U8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0xFF, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_I8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0x7F, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_I8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0x7F, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_U16: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_U16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0xFFFF, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_U16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0xFFFF, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_I16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0x7FFF, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_I16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0x7FFF, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_SIZE_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_OSIZE: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_U32: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_U32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    switch (cs->arch) {
    case TARGET_ARCH_Y86:
      break;
    case TARGET_ARCH_X64: {
      gp_gen_mov_reg(f, GP_D, GP_A);
      gp_gen_mov_reg_imm32(f, GP_C, 32);
      ia_gen_shr_cl(f, GP_D, OZ_64);
      gen_crash_jcc(f, h, IA_JCC_NE);
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_U32_TO_U32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
  } break;
  case PRIMITIVE_OP_CONVERT_SIZE_TO_I32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0x7FFFFFFF, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_U32_TO_I32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_S);
  } break;

  case PRIMITIVE_OP_CONVERT_I32_TO_U8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0xFF, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_I8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0x7F, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_G);
    ia_gen_cmp_imm(f, GP_A, -0x80, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_I8: {
    ia_gen_movsx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0x7F, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_G);
    ia_gen_cmp_imm(f, GP_A, -0x80, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_U16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0xFFFF, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);
  } break;
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_I16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_cmp_imm(f, GP_A, 0x7FFF, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_G);
    ia_gen_cmp_imm(f, GP_A, -0x8000, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_I16: {
    ia_gen_movsx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_cmp_imm(f, GP_A, 0x7FFF, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_G);
    ia_gen_cmp_imm(f, GP_A, -0x8000, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_L);
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_SIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_U32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_S);
  } break;
  case PRIMITIVE_OP_CONVERT_OSIZE_TO_I32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    switch (cs->arch) {
    case TARGET_ARCH_Y86:
      break;
    case TARGET_ARCH_X64: {
      gp_gen_mov_reg(f, GP_D, GP_A);
      x64_gen_sub_w64_imm32(f, X64_RDX, -0x8000000ll);
      gp_gen_mov_reg_imm32(f, GP_C, 32);
      ia_gen_shr_cl(f, GP_D, OZ_64);
      gen_crash_jcc(f, h, IA_JCC_NE);
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  case PRIMITIVE_OP_CONVERT_I32_TO_OSIZE: /* fallthrough */
  case PRIMITIVE_OP_CONVERT_I32_TO_I32: {
    ia_gen_movsx(f, GP_A, GP_BP, off0, OZ_32);
  } break;

  case PRIMITIVE_OP_NEGATE_I8: {
    gp_gen_movsx(f, GP_A, GP_BP, off0, OZ_8);
    /* TODO: (Also in s2.) For this and the other negations, can't we
    just check OF after the fact?  I missed that in the docs on the
    first read? */
    /* Crashes if the value is INT8_MIN by subtracting 1 and
    overflowing. */
    ia_gen_cmp_imm(f, GP_A, 1, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_O);
    ia_gen_neg(f, GP_A, OZ_8);
  } break;
  case PRIMITIVE_OP_NEGATE_I16: {
    gp_gen_movsx(f, GP_A, GP_BP, off0, OZ_16);
    /* Crashes if the value is INT16_MIN by subtracting 1 and
    overflowing. */
    ia_gen_cmp_imm(f, GP_A, 1, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_O);
    ia_gen_neg(f, GP_A, OZ_16);
  } break;
  case PRIMITIVE_OP_NEGATE_I32: {
    ia_gen_movsx(f, GP_A, GP_BP, off0, OZ_32);
    /* Crashes if the value is INT32_MIN by subtracting 1 and
    overflowing. */
    ia_gen_cmp_imm(f, GP_A, 1, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_O);
    ia_gen_neg(f, GP_A, OZ_32);
  } break;

  case PRIMITIVE_OP_LOGICAL_NOT: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_8);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_8);
    gp_gen_setcc_b8(f, GP_A, IA_SETCC_Z);
  } break;

  case PRIMITIVE_OP_BIT_NOT_I8: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_U8: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_8);
    ia_gen_not(f, GP_A, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_NOT_I16: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_U16: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_16);
    ia_gen_not(f, GP_A, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_NOT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_OSIZE: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, ptr_oz(f));
    ia_gen_not(f, GP_A, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_BIT_NOT_I32: /* fallthrough */
  case PRIMITIVE_OP_BIT_NOT_U32: {
    gp_gen_movzx(f, GP_A, GP_BP, off0, OZ_32);
    ia_gen_not(f, GP_A, OZ_32);
  } break;

  case PRIMITIVE_OP_EQ_PTR: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_NE_PTR: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, ptr_oz(f));
  } break;

  case PRIMITIVE_OP_ADD_U8: {
    gen_add_primop(f, h, off0, off1, OZ_8, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_U8: {
    gen_sub_primop(f, h, off0, off1, OZ_8, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_alah_mul_w8(f, X86_CL);
    gen_crash_jcc(f, h, IA_JCC_C);
    ia_gen_movzx8_reg8(f, GP_A, X86_AL);
  } break;
  case PRIMITIVE_OP_DIV_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_alah_div_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    ia_gen_movzx8_reg8(f, GP_A, X86_AL);
  } break;
  case PRIMITIVE_OP_MOD_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_alah_div_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    ia_gen_mov_reg8(f, X86_AL, X86_AH);
    ia_gen_movzx8_reg8(f, GP_A, X86_AL);
  } break;
  case PRIMITIVE_OP_LT_BOOL: /* fallthrough */
  case PRIMITIVE_OP_LT_U8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_B, OZ_8);
  } break;
  case PRIMITIVE_OP_LE_BOOL: /* fallthrough */
  case PRIMITIVE_OP_LE_U8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_BE, OZ_8);
  } break;
  case PRIMITIVE_OP_GT_BOOL: /* fallthrough */
  case PRIMITIVE_OP_GT_U8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_A, OZ_8);
  } break;
  case PRIMITIVE_OP_GE_BOOL: /* fallthrough */
  case PRIMITIVE_OP_GE_U8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_AE, OZ_8);
  } break;
  case PRIMITIVE_OP_EQ_BOOL: /* fallthrough */
  case PRIMITIVE_OP_EQ_U8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, OZ_8);
  } break;
  case PRIMITIVE_OP_NE_BOOL: /* fallthrough */
  case PRIMITIVE_OP_NE_U8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_XOR_BOOL: /* fallthrough */
  case PRIMITIVE_OP_BIT_XOR_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_xor(f, GP_A, GP_C, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_OR_BOOL: /* fallthrough */
  case PRIMITIVE_OP_BIT_OR_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_or(f, GP_A, GP_C, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_AND_BOOL: /* fallthrough */
  case PRIMITIVE_OP_BIT_AND_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_and(f, GP_A, GP_C, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 7, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U8: {
    gen_movzx_ac(f, off0, off1, OZ_8);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 7, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shr_cl(f, GP_A, OZ_8);
  } break;

  case PRIMITIVE_OP_ADD_I8: {
    gen_add_primop(f, h, off0, off1, OZ_8, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_SUB_I8: {
    gen_sub_primop(f, h, off0, off1, OZ_8, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_MUL_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_alah_imul_w8(f, X86_CL);
    gen_crash_jcc(f, h, IA_JCC_O);
    ia_gen_movzx8_reg8(f, GP_A, X86_AL);
  } break;
  case PRIMITIVE_OP_DIV_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_alah_idiv_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    ia_gen_movzx8_reg8(f, GP_A, X86_AL);
  } break;
  case PRIMITIVE_OP_MOD_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_alah_idiv_w8(f, X86_CL);
    /* Divide by zero will produce #DE. (I guess.) */
    ia_gen_mov_reg8(f, X86_AL, X86_AH);
    ia_gen_movzx8_reg8(f, GP_A, X86_AL);
  } break;
  case PRIMITIVE_OP_LT_I8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_L, OZ_8);
  } break;
  case PRIMITIVE_OP_LE_I8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_LE, OZ_8);
  } break;
  case PRIMITIVE_OP_GT_I8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_G, OZ_8);
  } break;
  case PRIMITIVE_OP_GE_I8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_GE, OZ_8);
  } break;
  case PRIMITIVE_OP_EQ_I8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, OZ_8);
  } break;
  case PRIMITIVE_OP_NE_I8: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_XOR_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_xor(f, GP_A, GP_C, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_OR_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_or(f, GP_A, GP_C, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_AND_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);
    ia_gen_and(f, GP_A, GP_C, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 7, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, OZ_8);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_I8: {
    gen_movzx_ac(f, off0, off1, OZ_8);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 7, OZ_8);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_sar_cl(f, GP_A, OZ_8);
  } break;

  case PRIMITIVE_OP_ADD_U16: {
    gen_add_primop(f, h, off0, off1, OZ_16, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_U16: {
    gen_sub_primop(f, h, off0, off1, OZ_16, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_azdz_mul(f, GP_C, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_DIV_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_xor(f, GP_D, GP_D, ptr_oz(f));
    ia_gen_azdz_idiv(f, GP_C, OZ_16);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_xor(f, GP_D, GP_D, ptr_oz(f));
    ia_gen_azdz_div(f, GP_C, OZ_16);
    /* Divide by zero will produce #DE. (I guess.) */
    ia_gen_mov(f, GP_A, GP_D, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_LT_U16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_B, OZ_16);
  } break;
  case PRIMITIVE_OP_LE_U16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_BE, OZ_16);
  } break;
  case PRIMITIVE_OP_GT_U16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_A, OZ_16);
  } break;
  case PRIMITIVE_OP_GE_U16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_AE, OZ_16);
  } break;
  case PRIMITIVE_OP_EQ_U16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, OZ_16);

  } break;
  case PRIMITIVE_OP_NE_U16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_XOR_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_xor(f, GP_A, GP_C, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_OR_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_or(f, GP_A, GP_C, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_AND_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_and(f, GP_A, GP_C, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 15, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U16: {
    gen_movzx_ac(f, off0, off1, OZ_16);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 15, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shr_cl(f, GP_A, OZ_16);
  } break;

  case PRIMITIVE_OP_ADD_I16: {
    gen_add_primop(f, h, off0, off1, OZ_16, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_SUB_I16: {
    gen_sub_primop(f, h, off0, off1, OZ_16, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_MUL_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_imul(f, GP_A, GP_C, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_DIV_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_cwdqo(f, OZ_16);
    ia_gen_azdz_idiv(f, GP_C, OZ_16);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_cwdqo(f, OZ_16);
    ia_gen_azdz_idiv(f, GP_C, OZ_16);
    ia_gen_mov(f, GP_A, GP_D, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_LT_I16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_L, OZ_16);
  } break;
  case PRIMITIVE_OP_LE_I16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_LE, OZ_16);
  } break;
  case PRIMITIVE_OP_GT_I16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_G, OZ_16);
  } break;
  case PRIMITIVE_OP_GE_I16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_GE, OZ_16);
  } break;
  case PRIMITIVE_OP_EQ_I16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, OZ_16);
  } break;
  case PRIMITIVE_OP_NE_I16: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_XOR_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_xor(f, GP_A, GP_C, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_OR_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_or(f, GP_A, GP_C, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_AND_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);
    ia_gen_and(f, GP_A, GP_C, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 15, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, OZ_16);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_I16: {
    gen_movzx_ac(f, off0, off1, OZ_16);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 15, OZ_16);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_sar_cl(f, GP_A, OZ_16);
  } break;

  case PRIMITIVE_OP_ADD_SIZE: {
    gen_add_primop(f, h, off0, off1, ptr_oz(f), IA_JCC_C);
  } break;
  case PRIMITIVE_OP_ADD_U32: {
    gen_add_primop(f, h, off0, off1, OZ_32, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_SIZE: {
    gen_sub_primop(f, h, off0, off1, ptr_oz(f), IA_JCC_C);
  } break;
  case PRIMITIVE_OP_SUB_U32: {
    gen_sub_primop(f, h, off0, off1, OZ_32, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_SIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_azdz_mul(f, GP_C, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_MUL_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_azdz_mul(f, GP_C, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_C);
  } break;
  case PRIMITIVE_OP_DIV_SIZE: /* fallthrough */
  case PRIMITIVE_OP_DIV_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_xor(f, GP_D, GP_D, ptr_oz(f));
    ia_gen_azdz_div(f, GP_C, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_DIV_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_xor(f, GP_D, GP_D, ptr_oz(f));
    ia_gen_azdz_div(f, GP_C, OZ_32);
    /* Divide by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_MOD_SIZE: /* fallthrough */
  case PRIMITIVE_OP_MOD_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_xor(f, GP_D, GP_D, ptr_oz(f));
    ia_gen_azdz_div(f, GP_C, ptr_oz(f));
    gp_gen_mov_reg(f, GP_A, GP_D);
  } break;
  case PRIMITIVE_OP_MOD_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_xor(f, GP_D, GP_D, ptr_oz(f));
    ia_gen_azdz_div(f, GP_C, OZ_32);
    gp_gen_mov_reg(f, GP_A, GP_D);
    /* Modulus by zero will produce #DE. (I guess.) */
  } break;
  case PRIMITIVE_OP_LT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_LT_OSIZE: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_B, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_LT_U32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_B, OZ_32);
  } break;
  case PRIMITIVE_OP_LE_SIZE: /* fallthrough */
  case PRIMITIVE_OP_LE_OSIZE: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_BE, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_LE_U32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_BE, OZ_32);
  } break;
  case PRIMITIVE_OP_GT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_GT_OSIZE: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_A, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_GT_U32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_A, OZ_32);
  } break;
  case PRIMITIVE_OP_GE_SIZE: /* fallthrough */
  case PRIMITIVE_OP_GE_OSIZE: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_AE, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_GE_U32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_AE, OZ_32);
  } break;
  case PRIMITIVE_OP_EQ_SIZE: /* fallthrough */
  case PRIMITIVE_OP_EQ_OSIZE: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_EQ_U32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, OZ_32);
  } break;
  case PRIMITIVE_OP_NE_SIZE: /* fallthrough */
  case PRIMITIVE_OP_NE_OSIZE: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_NE_U32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_XOR_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_XOR_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_xor(f, GP_A, GP_C, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_BIT_XOR_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_xor(f, GP_A, GP_C, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_OR_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_OR_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_or(f, GP_A, GP_C, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_BIT_OR_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_or(f, GP_A, GP_C, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_AND_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_AND_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_and(f, GP_A, GP_C, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_BIT_AND_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_and(f, GP_A, GP_C, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_LEFTSHIFT_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, ptr_size_bits(f) - 1, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 31, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_SIZE: /* fallthrough */
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, ptr_size_bits(f) - 1, ptr_oz(f));
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shr_cl(f, GP_A, ptr_oz(f));
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_U32: {
    gen_movzx_ac(f, off0, off1, OZ_32);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 31, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shr_cl(f, GP_A, OZ_32);
  } break;

  case PRIMITIVE_OP_ADD_I32: {
    gen_add_primop(f, h, off0, off1, OZ_32, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_SUB_I32: {
    gen_sub_primop(f, h, off0, off1, OZ_32, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_MUL_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_imul(f, GP_A, GP_C, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_O);
  } break;
  case PRIMITIVE_OP_DIV_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_cwdqo(f, OZ_32);
    ia_gen_azdz_idiv(f, GP_C, OZ_32);
    /* Divide by zero or INT32_MIN / -1 will produce #DE. */
  } break;
  case PRIMITIVE_OP_MOD_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_cwdqo(f, OZ_32);
    ia_gen_azdz_idiv(f, GP_C, OZ_32);
    gp_gen_mov_reg(f, GP_A, GP_D);
    /* Divide by zero or INT32_MIN / -1 will produce #DE. */
  } break;
  case PRIMITIVE_OP_LT_I32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_L, OZ_32);
  } break;
  case PRIMITIVE_OP_LE_I32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_LE, OZ_32);
  } break;
  case PRIMITIVE_OP_GT_I32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_G, OZ_32);
  } break;
  case PRIMITIVE_OP_GE_I32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_GE, OZ_32);
  } break;
  case PRIMITIVE_OP_EQ_I32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_E, OZ_32);
  } break;
  case PRIMITIVE_OP_NE_I32: {
    gen_cmp_behavior(f, off0, off1, IA_SETCC_NE, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_XOR_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_xor(f, GP_A, GP_C, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_OR_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_or(f, GP_A, GP_C, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_AND_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);
    ia_gen_and(f, GP_A, GP_C, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_LEFTSHIFT_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 31, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_shl_cl(f, GP_A, OZ_32);
  } break;
  case PRIMITIVE_OP_BIT_RIGHTSHIFT_I32: {
    gen_movzx_ac(f, off0, off1, OZ_32);

    /* We handle out-of-range rhs, that's all. */
    ia_gen_cmp_imm(f, GP_C, 31, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_A);

    ia_gen_sar_cl(f, GP_A, OZ_32);
  } break;

  case PRIMITIVE_OP_ADD_OSIZE: {
    gen_add_primop(f, h, off0, off1, ptr_oz(f), 0);
  } break;
  case PRIMITIVE_OP_SUB_OSIZE: {
    gen_sub_primop(f, h, off0, off1, ptr_oz(f), 0);
  } break;
  case PRIMITIVE_OP_MUL_OSIZE: {
    gen_movzx_ac(f, off0, off1, ptr_oz(f));
    ia_gen_azdz_mul(f, GP_C, ptr_oz(f));
  } break;

  default:
    UNREACHABLE();
  }
}

void gen_primitive_op_behavior(struct checkstate *cs,
                               struct objfile *f,
                               struct frame *h,
                               struct primitive_op prim_op,

                               /* is NULL if op has arity 0 */
                               struct ast_typeexpr *arg0_type_or_null,

                               int32_t callsite_base_offset,
                               struct funcall_arglist_info *arglist_info,
                               struct loc return_loc) {
  /* TODO(): x86: callsite_base_offset needs to be used for x64 -- on x86 assert that it equals stack offset. */
  switch (prim_op.tag) {
  case PRIMITIVE_OP_ENUMCONSTRUCT: {
    CHECK(arg0_type_or_null);
    struct loc arg0_loc = caller_arg_loc(callsite_base_offset, arglist_info, 0);
    gen_enumconstruct_behavior(cs, f, h, prim_op.u.enumconstruct_number,
                               arg0_type_or_null,
                               return_loc,
                               arg0_loc);
  } break;
  default: {
    gen_very_primitive_op_behavior(cs, f, h, prim_op, arg0_type_or_null,
                                   callsite_base_offset, arglist_info);
    postcall_return_in_loc(cs, f, arglist_info, return_loc);
  } break;
  }
}

int platform_can_return_in_eaxedx(struct checkstate *cs) {
  switch (cs->platform) {
  case TARGET_PLATFORM_OSX_32BIT: /* fallthrough */
  case TARGET_PLATFORM_WIN_32BIT:
    return 1;
  case TARGET_PLATFORM_LINUX_32BIT:
    return 0;
  case TARGET_PLATFORM_LINUX_64BIT:
    TODO_IMPLEMENT;
    break;
  case TARGET_PLATFORM_OSX_64BIT:
    TODO_IMPLEMENT;
    break;
  default:
    UNREACHABLE();
  }
}

/* Used to precompute arglist size, so that we can align the stack to
16-byte boundary at the function call. */
/* chase mark */
void y86_get_funcall_arglist_info(struct checkstate *cs,
                                  struct ast_typeexpr *return_type,
                                  struct ast_typeexpr *args,
                                  size_t args_count,
                                  struct funcall_arglist_info *info_out) {
  /* We specifically process our calculations _up_ from the call site
  -- if fields ever need alignment calculations, we'll be ready. */
  uint32_t return_type_size;
  int hidden_return_param
    = exists_hidden_return_param(cs, return_type, &return_type_size);

  struct funcall_arg_info *infos = malloc_mul(sizeof(*infos), args_count);

  uint32_t total_size = (hidden_return_param ? Y86_DWORD_SIZE : 0);
  for (size_t i = 0; i < args_count; i++) {
    uint32_t arg_size = gp_sizeof(&cs->nt, &args[i]);
    uint32_t padded_size = frame_padded_push_size(cs->arch, arg_size);
    infos[i].first_register = -1;
    infos[i].relative_disp = uint32_to_int32(total_size);
    infos[i].arg_size = arg_size;
    infos[i].padded_size = padded_size;
    /* No alignment concerns, (yet!)! */
    total_size = uint32_add(total_size, padded_size);
  }

  funcall_arglist_info_init(info_out, infos, args_count,
                            return_type_size, hidden_return_param,
                            total_size, 0);
}

void x64_get_funcall_arglist_info(struct checkstate *cs,
                                  struct ast_typeexpr *return_type,
                                  struct ast_typeexpr *args,
                                  size_t args_count,
                                  struct funcall_arglist_info *info_out) {
  uint32_t return_type_size;
  int hidden_return_param
    = exists_hidden_return_param(cs, return_type, &return_type_size);

  struct funcall_arg_info *infos = malloc_mul(sizeof(*infos), args_count);

  /* Offset of arg locations (non-negative values) relative to callsite. */
  int32_t offset = 0;
  /* Bottom offset of where register arg locations (non-positive
  values) are placed, relative to the HRP or return_loc.  (When we do a
  funcall, it's relative to callsite.  If we use this information to
  dump register params, it'll be relative to the callee's ebp
  offset.) */
  int32_t neg_offset = 0;

  int registers_used = hidden_return_param ? 1 : 0;
  for (size_t i = 0; i < args_count; i++) {
    uint32_t arg_size;
    int memory_param = x64_sysv_memory_param(cs, &args[i], &arg_size);

    uint32_t padded_size = uint32_ceil_aligned(arg_size, X64_EIGHTBYTE_SIZE);
    if (memory_param || registers_used + (arg_size > 8 ? 2 : 1) > 6) {
      /* Padded as the sysv x64 callconv goes. */
      infos[i].first_register = -1;
      infos[i].relative_disp = offset;
      infos[i].arg_size = arg_size;
      infos[i].padded_size = padded_size;
      offset = int32_add(offset, uint32_to_int32(padded_size));
    } else {
      neg_offset = int32_sub(neg_offset, uint32_to_int32(padded_size));
      infos[i].first_register = registers_used;
      infos[i].relative_disp = neg_offset;
      infos[i].arg_size = arg_size;
      infos[i].padded_size = padded_size;
      registers_used += 1 + (arg_size > 8);
    }
  }

  funcall_arglist_info_init(info_out, infos, args_count,
                            return_type_size, hidden_return_param,
                            offset, neg_offset);
}

/* y86/x64 */
/* chase mark */
void get_funcall_arglist_info(struct checkstate *cs,
                              struct ast_typeexpr *return_type,
                              struct ast_typeexpr *args,
                              size_t args_count,
                              struct funcall_arglist_info *info_out) {
  switch (cs->arch) {
  case TARGET_ARCH_Y86: {
    y86_get_funcall_arglist_info(cs, return_type, args, args_count, info_out);
  } break;
  case TARGET_ARCH_X64: {
    x64_get_funcall_arglist_info(cs, return_type, args, args_count, info_out);
  } break;
  default:
    UNREACHABLE();
  }
}

void x64_load_register_params(struct objfile *f, struct funcall_arglist_info *arglist_info,
                              int32_t callsite_base_offset, struct loc return_loc) {
  for (size_t i = 0, e = arglist_info->args_count; i < e; i++) {
    struct funcall_arg_info arg_info = arglist_info->arg_infos[i];
    if (arg_info.first_register != -1) {
      CHECK(arg_info.first_register >= 0 && arg_info.first_register < 6);
      struct loc arg_loc = caller_arg_loc(callsite_base_offset, arglist_info, i);

      x64_gen_load_register(f, x64_param_regs[arg_info.first_register],
                            ebp_loc(uint32_min(arg_loc.size, X64_EIGHTBYTE_SIZE),
                                    uint32_min(arg_loc.padded_size, X64_EIGHTBYTE_SIZE),
                                    arg_loc.u.ebp_offset));
      if (arg_loc.size > X64_EIGHTBYTE_SIZE) {
        CHECK(arg_loc.size <= 2 * X64_EIGHTBYTE_SIZE);
        CHECK(arg_loc.padded_size <= 2 * X64_EIGHTBYTE_SIZE);
        CHECK(arg_info.first_register < 5);
        x64_gen_load_register(f, x64_param_regs[arg_info.first_register + 1],
                              ebp_loc(uint32_sub(arg_loc.size, X64_EIGHTBYTE_SIZE),
                                      uint32_sub(arg_loc.padded_size, X64_EIGHTBYTE_SIZE),
                                      int32_add(arg_loc.u.ebp_offset, X64_EIGHTBYTE_SIZE)));
      }
    }
  }

  if (arglist_info->hidden_return_param) {
    x64_gen_load_addressof(f, X64_RDI, return_loc);
  }
}

void y86_postcall_return_in_loc(struct checkstate *cs,
                                struct objfile *f,
                                struct funcall_arglist_info *arglist_info,
                                struct loc return_loc) {
  if (!arglist_info->hidden_return_param) {
    if (arglist_info->return_type_size == 0) {
      /* nothing */
    } else if (arglist_info->return_type_size <= Y86_DWORD_SIZE) {
      /* Return value in eax. */
      gp_gen_store_register(f, return_loc, GP_A);
    } else {
      CHECK(platform_can_return_in_eaxedx(cs));
      CHECK(arglist_info->return_type_size == 2 * Y86_DWORD_SIZE);
      x86_gen_store_biregister(f, return_loc, X86_EAX, X86_EDX);
    }
  }
}

void x64_postcall_return_in_loc(struct objfile *f,
                                struct funcall_arglist_info *arglist_info,
                                struct loc return_loc) {

  if (!arglist_info->hidden_return_param) {
    if (arglist_info->return_type_size == 0) {
      /* nothing */
    } else if (arglist_info->return_type_size <= X64_EIGHTBYTE_SIZE) {
      x64_gen_store_register(f, return_loc, X64_RAX, X64_RCX);
    } else {
      CHECK(arglist_info->return_type_size <= 2 * X64_EIGHTBYTE_SIZE);
      x64_gen_store_biregister(f, ebp_loc(8, 8, return_loc.u.ebp_offset),
                               X64_RAX, X64_RDX, X64_RCX);
    }
  }
}

void postcall_return_in_loc(struct checkstate *cs,
                            struct objfile *f,
                            struct funcall_arglist_info *arglist_info,
                            struct loc return_loc) {
  switch (cs->arch) {
  case TARGET_ARCH_Y86:
    y86_postcall_return_in_loc(cs, f, arglist_info, return_loc);
    break;
  case TARGET_ARCH_X64:
    x64_postcall_return_in_loc(f, arglist_info, return_loc);
    break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
int gen_funcall_expr(struct checkstate *cs, struct objfile *f,
                     struct frame *h, struct ast_expr *a,
                     struct expr_return *er) {
  int ret;

  struct expr_return func_er = free_expr_return();
  /* TODO: We must use exprcatch information to see if we should free
  a temporary. */
  if (!gen_expr(cs, f, h, &a->u.funcall.func->expr, &func_er)) {
    ret = 0;
    goto fail;
  }

  struct ast_typeexpr *func_type = ast_expr_type(&a->u.funcall.func->expr);
  struct ast_typeexpr *args;
  size_t args_count;
  struct ast_typeexpr *return_type;
  expose_func_type_parts(&cs->cm, func_type, &args, &args_count, &return_type);
  CHECK(args_count == a->u.funcall.args_count);

  struct funcall_arglist_info arglist_info;
  get_funcall_arglist_info(cs, return_type, args, args_count, &arglist_info);

  /* y86/x64 */
  struct loc return_loc;
  if (er->tag == EXPR_RETURN_DEMANDED) {
    /* Return locations perhaps must be non-aliasable locations on the
    stack -- this checks that it's a stack location or our callee's
    hidden return param pointer. */
    CHECK(erd_loc(&er->u.demand).tag == LOC_EBP_OFFSET
          || (erd_loc(&er->u.demand).tag == LOC_EBP_INDIRECT
              && erd_loc(&er->u.demand).u.ebp_indirect == hrp_ebp_indirect(h->arch)));
    return_loc = erd_loc(&er->u.demand);
  } else {
    return_loc = frame_push_loc(h, arglist_info.return_type_size);
  }

  int32_t saved_offset = frame_save_offset(h);
  adjust_frame_for_callsite_alignment(h, arglist_info.total_size);
  frame_push_exact_amount(h, arglist_info.total_size);
  int32_t callsite_base_offset = frame_save_offset(h);
  frame_push_exact_amount(h, int32_to_uint32(int32_negate(arglist_info.neg_size)));
  int32_t arglist_neg_offset = frame_save_offset(h);

  for (size_t i = 0; i < args_count; i++) {
    /* Notably, the arg loc is correct whether it's put in a register
    (with negative offset) or not. */
    struct loc arg_loc = caller_arg_loc(callsite_base_offset, &arglist_info, i);

    /* TODO: We must use ast_exprcatch information to force the
    temporary in arg_loc. */
    /* (Right now, EXPR_RETURN_DEMANDED is known to work this way only
    when it encounters another funcall.) */
    struct ast_expr *arg = &a->u.funcall.args[i].expr;
    struct expr_return arg_er = demand_expr_return(arg_loc);
    if (!gen_expr(cs, f, h, arg, &arg_er)) {
      ret = 0;
      goto fail_arglist_info;
    }
    frame_restore_offset(h, arglist_neg_offset);
  }

  switch (cs->arch) {
  case TARGET_ARCH_Y86: {
    if (arglist_info.hidden_return_param) {
      struct loc ptr_loc = ebp_loc(Y86_DWORD_SIZE, Y86_DWORD_SIZE, callsite_base_offset);
      gen_mov_addressof(f, ptr_loc, return_loc);
    }

    frame_restore_offset(h, callsite_base_offset);

    switch (func_er.u.free.tag) {
    case EXPR_RETURN_FREE_IMM: {
      gen_call_imm(cs, f, h, func_er.u.free.u.imm, arglist_info.hidden_return_param);
      y86_postcall_return_in_loc(cs, f, &arglist_info, return_loc);
    } break;
    case EXPR_RETURN_FREE_LOC: {
      struct loc func_loc;
      wipe_temporaries(cs, f, h, &func_er, func_type, &func_loc);
      gp_gen_load_register(f, GP_A, func_loc);
      gen_placeholder_stack_adjustment(f, h, 0);
      x86_gen_indirect_call_reg(f, X86_EAX);
      if (arglist_info.hidden_return_param && platform_ret4_hrp(cs)) {
        /* TODO: We could do this more elegantly, but right now undo the
        callee's pop of esp. */
        x86_gen_add_esp_i32(f, -4);
      }
      gen_placeholder_stack_adjustment(f, h, 1);
      y86_postcall_return_in_loc(cs, f, &arglist_info, return_loc);
    } break;
    case EXPR_RETURN_FREE_PRIMITIVE_OP: {
      gen_primitive_op_behavior(cs, f, h, func_er.u.free.u.primitive_op,
                                (args_count == 0 ? NULL : &args[0]),
                                callsite_base_offset,
                                &arglist_info,
                                return_loc);
    } break;
    default:
      UNREACHABLE();
    }
  } break;

  case TARGET_ARCH_X64: {
    switch (func_er.u.free.tag) {
    case EXPR_RETURN_FREE_IMM: {
      x64_load_register_params(f, &arglist_info, callsite_base_offset, return_loc);
      frame_restore_offset(h, callsite_base_offset);
      gen_call_imm(cs, f, h, func_er.u.free.u.imm, arglist_info.hidden_return_param);
      x64_postcall_return_in_loc(f, &arglist_info, return_loc);
    } break;
    case EXPR_RETURN_FREE_LOC: {
      struct loc func_loc;
      wipe_temporaries(cs, f, h, &func_er, func_type, &func_loc);

      x64_load_register_params(f, &arglist_info, callsite_base_offset, return_loc);
      frame_restore_offset(h, callsite_base_offset);

      gp_gen_load_register(f, GP_A, func_loc);
      gen_placeholder_stack_adjustment(f, h, 0);
      x64_gen_indirect_call_reg(f, X64_RAX);
      gen_placeholder_stack_adjustment(f, h, 1);
      x64_postcall_return_in_loc(f, &arglist_info, return_loc);
    } break;
    case EXPR_RETURN_FREE_PRIMITIVE_OP: {
      gen_primitive_op_behavior(cs, f, h, func_er.u.free.u.primitive_op,
                                (args_count == 0 ? NULL : &args[0]),
                                callsite_base_offset,
                                &arglist_info,
                                return_loc);
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  default:
    UNREACHABLE();
  }

  if (er->tag != EXPR_RETURN_DEMANDED) {
    expr_return_set(cs, f, h, er, return_loc, return_type,
                    temp_exists(return_loc, return_type, 1));
  } else {
    /* TODO: Icky.  x64 case identically icky. */
    er_set_tr(er, temp_exists(erd_loc(&er->u.demand), return_type, 1));
  }

  frame_restore_offset(h, saved_offset);

  ret = 1;
 fail_arglist_info:
  funcall_arglist_info_destroy(&arglist_info);
 fail:
  return ret;
}

/* chase mark */
void apply_dereference(struct checkstate *cs, struct objfile *f,
                       struct frame *h, struct loc ptr_loc,
                       uint32_t pointee_size, struct expr_return *er,
                       struct ast_typeexpr *type) {
  struct loc loc = frame_push_loc(h, ptr_size(cs->arch));
  gen_mov(f, loc, ptr_loc);
  CHECK(loc.tag == LOC_EBP_OFFSET);

  struct loc ret = ebp_indirect_loc(pointee_size, pointee_size,
                                    loc.u.ebp_offset);
  expr_return_set(cs, f, h, er, ret, type, temp_none());
}

/* chase mark */
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
    uint32_t size = gp_sizeof(&cs->nt, type);
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

    struct loc ret = frame_push_loc(h, ptr_size(cs->arch));
    gen_mov_addressof(f, ret, ero_loc(&rhs_er.u.open));
    expr_return_set(cs, f, h, er, ret, ast_expr_type(a),
                    temp_exists(ret, ast_expr_type(a), 1));
    return 1;
  } break;
  case AST_UNOP_NEGATE:
  case AST_UNOP_CONVERT:
  case AST_UNOP_UPCONVERT:
  case AST_UNOP_LOGICAL_NOT:
  case AST_UNOP_BITWISE_NOT:
  default:
    UNREACHABLE();
  }
}

/* chase mark */
int gen_index_expr(struct checkstate *cs, struct objfile *f,
                   struct frame *h, struct ast_expr *a,
                   struct expr_return *er) {
  struct ast_index_expr *ie = &a->u.index_expr;

  struct ast_typeexpr *ptr_target = NULL;
  int is_ptr = view_ptr_target(&cs->cm, ast_expr_type(ie->lhs), &ptr_target);

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

  uint32_t elem_size;
  if (is_ptr) {
    CHECK(ptr_target->tag == AST_TYPEEXPR_ARRAY);
    elem_size = gp_sizeof(&cs->nt, ptr_target->u.arraytype.param);

    CHECK(lhs_loc.size == ptr_size(cs->arch));
    CHECK(rhs_loc.size == size_size(cs->arch));

    gp_gen_load_register(f, GP_D, lhs_loc);
  } else {
    struct ast_typeexpr *lhs_type = ast_expr_type(ie->lhs);
    CHECK(lhs_type->tag == AST_TYPEEXPR_ARRAY);
    elem_size = gp_sizeof(&cs->nt, lhs_type->u.arraytype.param);

    CHECK(lhs_loc.size == uint32_mul(elem_size, unsafe_numeric_literal_u32(&lhs_type->u.arraytype.number)));
    CHECK(rhs_loc.size == size_size(cs->arch));

    gp_gen_load_addressof(f, GP_D, lhs_loc);
  }

  gp_gen_load_register(f, GP_A, rhs_loc);

  gp_gen_mov_reg_imm32(f, GP_C, elem_size);
  ia_gen_imul(f, GP_A, GP_C, ptr_oz(f));
  gen_crash_jcc(f, h, IA_JCC_O);

  ia_gen_add(f, GP_A, GP_D, ptr_oz(f));

  struct loc loc = frame_push_loc(h, ptr_size(h->arch));
  gp_gen_store_register(f, loc, GP_A);

  struct loc retloc = ebp_indirect_loc(elem_size, elem_size, loc.u.ebp_offset);
  if (is_ptr) {
    expr_return_set(cs, f, h, er, retloc, ast_expr_type(a), temp_none());
  } else {
    expr_return_set(cs, f, h, er, retloc, ast_expr_type(a),
                    temp_subobject(*er_tr(&lhs_er)));
  }
  return 1;
}

/* chase mark */
void gen_placeholder_jmp_if_false(struct objfile *f, struct frame *h,
                                  struct loc loc, size_t target_number) {
  CHECK(loc.size == KIT_BOOL_SIZE);

  gp_gen_load_register(f, GP_A, loc);
  gp_gen_test_regs(f, GP_A, GP_A, OZ_8);
  gen_placeholder_jcc(f, h, IA_JCC_Z, target_number);
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

/* chase mark */
void gen_crash_jcc(struct objfile *f, struct frame *h, enum ia_jcc code) {
  gen_placeholder_jcc(f, h, code, frame_crash_target_number(h));
}

void gen_crash_jmp(struct objfile *f, struct frame *h) {
  gen_placeholder_jmp(f, h, frame_crash_target_number(h));
}

/* chase mark */
void gen_placeholder_jmp(struct objfile *f, struct frame *h, size_t target_number) {
  struct jmpdata jd;
  jd.target_number = target_number;
  jd.jmp_location = 1 + objfile_section_size(objfile_text(f));
  SLICE_PUSH(h->jmpdata, h->jmpdata_count, h->jmpdata_limit, jd);
  /* y86/x64 */
  /* E9 jmp instruction */
  uint8_t b[5] = { 0xE9, 0, 0, 0, 0 };
  apptext(f, b, 5);
}

void replace_placeholder_jump(struct objfile *f, size_t jmp_location,
                              size_t target_offset) {
  /* y86/x64 - we use same jmp instructions. */
  int32_t target32 = size_to_int32(target_offset);
  int32_t jmp32 = size_to_int32(size_add(jmp_location, 4));
  int32_t diff = int32_sub(target32, jmp32);
  char buf[4];
  write_le_i32(buf, diff);
  objfile_section_overwrite_raw(objfile_text(f),
                                jmp_location,
                                buf,
                                sizeof(buf));
}

/* chase mark */
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
  gp_gen_load_addressof(f, GP_C, lhs_loc);
  gp_gen_load_addressof(f, GP_D, rhs_loc);
  ia_gen_cmp(f, GP_D, GP_C, ptr_oz(f));
  gen_placeholder_jcc(f, h, IA_JCC_Z, target_number);

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

/* chase mark */
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

    struct loc ret = frame_push_loc(h, KIT_BOOL_SIZE);

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

    struct loc ret = frame_push_loc(h, KIT_BOOL_SIZE);

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

/* chase mark */
void expr_return_immediate(struct objfile *f, struct frame *h,
                           struct expr_return *er,
                           struct immediate imm) {
  switch (er->tag) {
  case EXPR_RETURN_DEMANDED: {
    gen_mov_immediate(f, erd_loc(&er->u.demand), imm);
    er_set_tr(er, temp_exists_trivial(erd_loc(&er->u.demand), 1));
  } break;
  case EXPR_RETURN_OPEN: {
    struct loc floc = frame_push_loc(h, immediate_size(h->arch, imm));
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

/* chase mark */
void expr_return_primitive_op(struct expr_return *er,
                              struct primitive_op primitive_op) {
  switch (er->tag) {
  case EXPR_RETURN_DEMANDED: {
    CRASH("Trying to put primitive op into demanded expr_return.");
  } break;
  case EXPR_RETURN_OPEN: {
    CRASH("Trying to put primitive op into open expr_return.");
  } break;
  case EXPR_RETURN_FREE: {
    er->u.free.tag = EXPR_RETURN_FREE_PRIMITIVE_OP;
    er->u.free.u.primitive_op = primitive_op;
    er_set_tr(er, temp_primitive_op());
  } break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
int help_gen_immediate_numeric(struct checkstate *cs,
                               struct objfile *f,
                               struct frame *h,
                               struct ast_typeexpr *type,
                               struct ast_meta *meta,
                               uint32_t numeric_literal_value,
                               struct expr_return *er) {
  CHECK(type->tag == AST_TYPEEXPR_NAME);
  CHECK(is_numeric_type(cs->im, type));

  if (type->u.name.value == cs->cm.i32_type_name) {
    int32_t value;
    if (!squash_u32_to_i32(numeric_literal_value, &value)) {
      return 0;
    }

    struct immediate imm;
    imm.tag = IMMEDIATE_I32;
    imm.u.i32 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } else if (type->u.name.value == cs->cm.u32_type_name
             || type->u.name.value == cs->cm.size_type_name
             || type->u.name.value == cs->cm.osize_type_name) {
    expr_return_immediate(f, h, er, imm_u32(numeric_literal_value));
    return 1;
  } else if (type->u.name.value == cs->cm.u8_type_name) {
    uint8_t value;
    if (!squash_u32_to_u8(numeric_literal_value, &value)) {
      return 0;
    }
    struct immediate imm;
    imm.tag = IMMEDIATE_U8;
    imm.u.u8 = value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } else if (type->u.name.value == cs->cm.i8_type_name) {
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
    METERR(cs, *meta, "Compiler incomplete: Numeric literal resolves to type '%.*s', "
           "which this lame compiler cannot codegen for literals.\n",
           IM_P(cs->im, type->u.name.value));
    return 0;
  }
}

/* chase mark */
int gen_immediate_numeric_literal(struct checkstate *cs,
                                  struct objfile *f,
                                  struct frame *h,
                                  struct ast_typeexpr *type,
                                  struct ast_numeric_literal *a,
                                  struct expr_return *er) {
  CHECK(type->tag == AST_TYPEEXPR_NAME);
  CHECK(is_numeric_type(cs->im, type));

  /* TODO: Allow 64-bit numeric literals on 64-bit platforms. */
  uint32_t numeric_literal_value;
  if (!numeric_literal_to_u32(a, &numeric_literal_value)) {
    return 0;
  }

  return help_gen_immediate_numeric(cs, f, h, type, &a->meta,
                                    numeric_literal_value, er);
}

/* chase mark */
struct loc gen_array_element_loc(struct checkstate *cs,
                                 struct objfile *f,
                                 struct frame *h,
                                 struct loc src,
                                 struct ast_typeexpr *elem_type,
                                 uint32_t index) {
  uint32_t elem_size = gp_sizeof(&cs->nt, elem_type);
  uint32_t elem_offset = uint32_mul(elem_size, index);

  gp_gen_load_addressof(f, GP_D, src);
  gp_gen_mov_reg_imm32(f, GP_C, elem_offset);
  ia_gen_add(f, GP_D, GP_C, ptr_oz(f));
  struct loc loc = frame_push_loc(h, ptr_size(cs->arch));
  gp_gen_store_register(f, loc, GP_D);

  struct loc retloc = ebp_indirect_loc(elem_size, elem_size, loc.u.ebp_offset);
  return retloc;
}

/* chase mark */
struct loc gen_field_loc(struct checkstate *cs,
                         struct objfile *f,
                         struct frame *h,
                         struct loc lhs_loc,
                         struct ast_typeexpr *type,
                         ident_value fieldname) {
  /* Generally speaking: There's no way the field possibly gets a
  padded_size, because the first N fields of two struct types, if
  identical, need to be accessible when they're in a union without
  touching subsequent fields. */
  uint32_t size;
  uint32_t offset;
  gp_field_sizeoffset(&cs->nt, type, fieldname, &size, &offset);

  return gen_subobject_loc(f, h, lhs_loc, size, offset);
}

/* chase mark */
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
    struct loc field_ptr_loc = frame_push_loc(h, ptr_size(h->arch));
    gp_gen_mov_reg_stiptr(f, GP_A, loc.u.global_sti);
    gp_gen_lea(f, GP_A, GP_A, uint32_to_int32(offset));
    gp_gen_store(f, GP_BP, field_ptr_loc.u.ebp_offset, GP_A, ptr_oz(f));
    ret = ebp_indirect_loc(size, size, field_ptr_loc.u.ebp_offset);
  } break;
  case LOC_EBP_INDIRECT: {
    struct loc field_ptr_loc = frame_push_loc(h, ptr_size(h->arch));
    gp_gen_movzx(f, GP_A, GP_BP, loc.u.ebp_indirect, ptr_oz(f));
    gp_gen_lea(f, GP_A, GP_A, uint32_to_int32(offset));
    gp_gen_store(f, GP_BP, field_ptr_loc.u.ebp_offset, GP_A, ptr_oz(f));
    ret = ebp_indirect_loc(size, size, field_ptr_loc.u.ebp_offset);
  } break;
  default:
    UNREACHABLE();
  }

  return ret;
}

/* chase mark */
void apply_field_access(struct checkstate *cs,
                        struct objfile *f,
                        struct frame *h,
                        struct loc lhs_loc,
                        struct temp_return lhs_tr,
                        struct ast_typeexpr *type,
                        struct ast_fieldname *fieldname,
                        struct ast_typeexpr *field_type,
                        struct expr_return *er) {
  struct loc field_loc = gen_field_loc(cs, f, h, lhs_loc, type, fieldname->ident.value);
  expr_return_set(cs, f, h, er, field_loc, field_type, temp_subobject(lhs_tr));
}

/* chase mark */
int gen_local_field_access(struct checkstate *cs, struct objfile *f,
                           struct frame *h, struct ast_expr *a,
                           struct expr_return *er) {
  struct expr_return lhs_er = open_expr_return();
  if (!gen_expr(cs, f, h, a->u.local_field_access.lhs, &lhs_er)) {
    return 0;
  }
  struct ast_typeexpr *lhs_type = ast_expr_type(a->u.local_field_access.lhs);
  if (lhs_type->tag == AST_TYPEEXPR_ARRAY) {
    CHECK(a->u.local_field_access.fieldname.ident.value == cs->cm.array_length_fieldname);
    gen_destroy_temp(cs, f, h, *er_tr(&lhs_er));
    uint32_t lhs_arraytype_count
      = unsafe_numeric_literal_u32(&lhs_type->u.arraytype.number);
    /* X86 32-bit size specific. */
    expr_return_immediate(f, h, er, imm_u32(lhs_arraytype_count));
  } else {
    apply_field_access(cs, f, h, ero_loc(&lhs_er.u.open), *er_tr(&lhs_er),
                       lhs_type,
                       &a->u.local_field_access.fieldname,
                       ast_expr_type(a), er);
  }
  return 1;
}

/* chase mark */
int gen_deref_field_access(struct checkstate *cs, struct objfile *f,
                           struct frame *h, struct ast_expr *a,
                           struct expr_return *er) {
  struct loc lhs_loc;
  {
    struct expr_return lhs_er = open_expr_return();
    if (!gen_expr(cs, f, h, a->u.deref_field_access.lhs, &lhs_er)) {
      return 0;
    }
    wipe_temporaries(cs, f, h, &lhs_er, ast_expr_type(a->u.deref_field_access.lhs),
                     &lhs_loc);
  }

  struct ast_typeexpr *ptr_target;
  if (!view_ptr_target(&cs->cm, ast_expr_type(a->u.deref_field_access.lhs),
                       &ptr_target)) {
    CRASH("deref field access typechecked on a non-pointer.");
  }

  uint32_t full_size = gp_sizeof(&cs->nt, ptr_target);
  struct expr_return deref_er = open_expr_return();
  apply_dereference(cs, f, h, lhs_loc, full_size, &deref_er, ptr_target);

  /* An unimportant fact about a check we know. */
  CHECK(!er_tr(&deref_er)->exists);
  apply_field_access(cs, f, h, ero_loc(&deref_er.u.open), *er_tr(&deref_er),
                     ptr_target,
                     &a->u.deref_field_access.fieldname,
                     ast_expr_type(a), er);
  return 1;
}

/* chase mark */
void gen_inst_value(struct checkstate *cs, struct objfile *f, struct frame *h,
                    struct def_instantiation *inst, struct expr_return *er) {
  if (inst->value_computed && di_value(inst)->tag == STATIC_VALUE_PRIMITIVE_OP) {
    expr_return_primitive_op(er, di_value(inst)->u.primitive_op);
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
      uint32_t size = gp_sizeof(&cs->nt, &inst->type);
      /* TODO: Maybe globals' alignment rules are softer. */
      uint32_t padded_size = size;
      struct loc loc = global_loc(size, padded_size,
                                  di_symbol_table_index(inst));
      expr_return_set(cs, f, h, er, loc, &inst->type, temp_none());
    }
  }
}

/* chase mark */
int gen_strinit_expr(struct checkstate *cs, struct objfile *f,
                     struct frame *h, struct ast_expr *a,
                     struct expr_return *er) {
  struct ast_typeexpr *stype = ast_strinit_structish_type(&a->u.strinit);
  uint32_t size = gp_sizeof(&cs->nt, stype);
  struct loc loc = frame_push_loc(h, size);

  for (size_t i = 0, e = a->u.strinit.exprs_count; i < e; i++) {
    int32_t saved_offset = frame_save_offset(h);
    struct loc field_loc;
    if (stype->tag == AST_TYPEEXPR_STRUCTE) {
      CHECK(stype->u.structe.fields_count == a->u.strinit.exprs_count);
      field_loc = gen_field_loc(cs, f, h, loc, stype,
                                stype->u.structe.fields[i].name.value);
    } else {
      CHECK(stype->tag == AST_TYPEEXPR_ARRAY);
      CHECK(unsafe_numeric_literal_u32(&stype->u.arraytype.number) == size_to_uint32(a->u.strinit.exprs_count));
      field_loc = gen_array_element_loc(cs, f, h, loc, stype->u.arraytype.param, i);
    }
    struct expr_return field_er = demand_expr_return(field_loc);
    if (!gen_expr(cs, f, h, &a->u.strinit.exprs[i], &field_er)) {
      return 0;
    }
    frame_restore_offset(h, saved_offset);
  }

  expr_return_set(cs, f, h, er, loc, stype,
                  temp_exists(loc, stype, 1));
  return 1;
}

/* chase mark */
int gen_string_literal(struct checkstate *cs, struct objfile *f,
                       struct frame *h, struct ast_expr *a,
                       struct expr_return *er) {
  uint32_t size = size_to_uint32(a->u.string_literal.values_count);
  struct sti symbol_table_index
    = add_data_string(cs, f, a->u.string_literal.values, size);

  struct loc loc = global_loc(size, size, symbol_table_index);
  expr_return_set(cs, f, h, er, loc, ast_expr_type(a), temp_none());
  return 1;
}

/* chase mark */
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
      expr_return_set(cs, f, h, er, h->vardata[vi].loc, h->vardata[vi].concrete_type,
                      temp_none());
    }
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL:
    return gen_immediate_numeric_literal(cs, f, h, ast_expr_type(a),
                                         &a->u.numeric_literal, er);
  case AST_EXPR_BOOL_LITERAL: {
    CHECK(a->u.bool_literal.value == 0 || a->u.bool_literal.value == 1);
    struct immediate imm;
    imm.tag = IMMEDIATE_U8;
    imm.u.u8 = (uint8_t)a->u.bool_literal.value;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_EXPR_NULL_LITERAL: {
    struct immediate imm;
    switch (ptr_size(h->arch)) {
    case 4:
      imm = imm_u32(0);
      break;
    case 8:
      imm = imm_u64(0);
      break;
    default:
      UNREACHABLE();
    }
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_EXPR_VOID_LITERAL: {
    struct immediate imm;
    imm.tag = IMMEDIATE_VOID;
    expr_return_immediate(f, h, er, imm);
    return 1;
  } break;
  case AST_EXPR_CHAR_LITERAL:
    return help_gen_immediate_numeric(cs, f, h, ast_expr_type(a),
                                      &a->u.char_literal.meta,
                                      (uint32_t)a->u.char_literal.value,
                                      er);
  case AST_EXPR_STRING_LITERAL:
    return gen_string_literal(cs, f, h, a, er);
  case AST_EXPR_FUNCALL:
    return gen_funcall_expr(cs, f, h, a, er);
  case AST_EXPR_INDEX:
    return gen_index_expr(cs, f, h, a, er);
  case AST_EXPR_UNOP:
    return gen_unop_expr(cs, f, h, a, er);
  case AST_EXPR_BINOP:
    return gen_binop_expr(cs, f, h, a, er);
  case AST_EXPR_LAMBDA: {
    /* Lambdas should be compiled to global functions.. separately. */
    TODO_IMPLEMENT;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS:
    return gen_local_field_access(cs, f, h, a, er);
  case AST_EXPR_DEREF_FIELD_ACCESS:
    return gen_deref_field_access(cs, f, h, a, er);
  case AST_EXPR_TYPED:
    return gen_expr(cs, f, h, a->u.typed_expr.expr, er);
  case AST_EXPR_STRINIT:
    return gen_strinit_expr(cs, f, h, a, er);
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
    if (vd->destroy_when_unwound) {
      gen_destroy(cs, f, h, vd->loc, vd->concrete_type);
    }
  }

  gen_placeholder_jmp(f, h, h->return_target_number);
}

int gen_bracebody(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_bracebody *a);

struct swartch_facts {
  struct ast_typeexpr *type;
  struct loc loc;
  struct loc enum_loc;
  size_t num_constructors;
};

/* chase mark */
int gen_swartch(struct checkstate *cs, struct objfile *f,
                struct frame *h, struct ast_expr *swartch,
                struct swartch_facts *out) {
  struct ast_typeexpr *swartch_type = ast_expr_type(swartch);
  struct loc swartch_loc = frame_push_loc(h, gp_sizeof(&cs->nt, swartch_type));

  {
    int32_t swartch_saved_offset = frame_save_offset(h);
    struct expr_return swartch_er = demand_expr_return(swartch_loc);

    if (!gen_expr(cs, f, h, swartch, &swartch_er)) {
      return 0;
    }
    frame_restore_offset(h, swartch_saved_offset);
  }

  struct loc enum_loc;
  {
    struct ast_typeexpr *target;
    if (view_ptr_target(&cs->cm, swartch_type, &target)) {
      CHECK(swartch_loc.tag == LOC_EBP_OFFSET);
      uint32_t size = gp_sizeof(&cs->nt, target);
      enum_loc = ebp_indirect_loc(size, size, swartch_loc.u.ebp_offset);
    } else {
      enum_loc = swartch_loc;
    }
  }

  {
    struct vardata vd;
    vardata_init(&vd, IDENT_VALUE_INVALID,
                 1, swartch_type, swartch_loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);
  }

  out->type = swartch_type;
  out->loc = swartch_loc;
  out->enum_loc = enum_loc;
  return 1;
}

/* chase mark */
void ungen_swartch(struct checkstate *cs, struct objfile *f,
                   struct frame *h, struct swartch_facts *facts) {
  gen_destroy(cs, f, h, facts->loc, facts->type);
}

/* chase mark */
int gen_casebody(struct checkstate *cs, struct objfile *f,
                 struct frame *h, struct swartch_facts *facts,
                 struct ast_constructor_pattern *constructor,
                 struct ast_bracebody *body,
                 size_t fail_target_number) {
  CHECK(4 == enum_tag_size(cs->arch));
  ia_gen_cmp_imm(f, GP_A,
                 int32_add(
                     size_to_int32(
                         ast_case_pattern_info_constructor_number(&constructor->info).value),
                     FIRST_ENUM_TAG_NUMBER),
                 OZ_32);
  gen_placeholder_jcc(f, h, IA_JCC_NE, fail_target_number);

  struct vardata vd;
  if (constructor->has_decl) {
    struct ast_typeexpr *var_type = ast_var_info_type(&constructor->decl_.var_info);
    struct type_attrs var_attrs = gp_attrsof(&cs->nt, var_type);
    struct loc var_loc = make_enum_body_loc(f, h, facts->enum_loc,
                                            var_attrs.size, var_attrs.align);

    /* We don't destroy the variable -- the swartch gets push/popped instead. */
    vardata_init(&vd, constructor->decl_.name.value, 0, var_type, var_loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);
  }

  if (!gen_bracebody(cs, f, h, body)) {
    return 0;
  }

  if (constructor->has_decl) {
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  }
  return 1;
}

struct condition_state {
  enum ast_condition_tag tag;
  union {
    struct loc expr_cond_loc;
    struct {
      struct swartch_facts facts;
      struct ast_constructor_pattern *constructor;
    } pattern;
  } u;
};

/* chase mark */
int gen_condition(struct checkstate *cs, struct objfile *f,
                  struct frame *h, struct ast_condition *a,
                  struct condition_state *out) {
  switch (a->tag) {
  case AST_CONDITION_EXPR: {
    struct ast_expr *expr = a->u.expr;
    struct expr_return cond_er = open_expr_return();
    if (!gen_expr(cs, f, h, expr, &cond_er)) {
      return 0;
    }
    out->tag = AST_CONDITION_EXPR;
    wipe_temporaries(cs, f, h, &cond_er, ast_expr_type(expr),
                     &out->u.expr_cond_loc);
    return 1;
  } break;
  case AST_CONDITION_PATTERN: {
    struct swartch_facts facts;
    if (!gen_swartch(cs, f, h, a->u.pa.rhs, &facts)) {
      return 0;
    }

    out->tag = AST_CONDITION_PATTERN;
    out->u.pattern.facts = facts;
    out->u.pattern.constructor = &a->u.pa.pattern;
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
void pop_cstate_vardata(struct frame *h, struct condition_state *cstate) {
  switch (cstate->tag) {
  case AST_CONDITION_EXPR: {
    /* No vardata. */
  } break;
  case AST_CONDITION_PATTERN: {
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
  } break;
  }
}

/* chase mark */
int gen_successbody(struct checkstate *cs, struct objfile *f,
                    struct frame *h, struct condition_state *cstate,
                    struct ast_bracebody *body,
                    int32_t before_condition_saved_offset,
                    size_t fail_target_number,
                    size_t end_target_number) {
  switch (cstate->tag) {
  case AST_CONDITION_EXPR: {
    gen_placeholder_jmp_if_false(f, h, cstate->u.expr_cond_loc, fail_target_number);
    frame_restore_offset(h, before_condition_saved_offset);
    if (!gen_bracebody(cs, f, h, body)) {
      return 0;
    }
    gen_placeholder_jmp(f, h, end_target_number);
    return 1;
  } break;
  case AST_CONDITION_PATTERN: {
    /* TODO(): Enum tag size presumption? */
    struct loc swartch_num_loc = make_enum_num_loc(f, h, cstate->u.pattern.facts.enum_loc);
    gp_gen_load_register(f, GP_A, swartch_num_loc);
    if (!gen_casebody(cs, f, h, &cstate->u.pattern.facts,
                      cstate->u.pattern.constructor, body,
                      fail_target_number)) {
      return 0;
    }
    ungen_swartch(cs, f, h, &cstate->u.pattern.facts);
    gen_placeholder_jmp(f, h, end_target_number);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
void gen_afterfail_condition_cleanup(struct checkstate *cs, struct objfile *f,
                                     struct frame *h, struct condition_state *cstate) {
  switch (cstate->tag) {
  case AST_CONDITION_EXPR: {
    /* No cleanup. */
  } break;
  case AST_CONDITION_PATTERN: {
    /* Check for zero-tag. */
    /* TODO: Check for other out-of-range cases. */
    CHECK(4 == enum_tag_size(cs->arch));
    struct loc swartch_num_loc
      = make_enum_num_loc(f, h, cstate->u.pattern.facts.enum_loc);
    gp_gen_load_register(f, GP_A, swartch_num_loc);
    STATIC_CHECK(FIRST_ENUM_TAG_NUMBER == 1);
    gp_gen_test_regs(f, GP_A, GP_A, OZ_32);
    gen_crash_jcc(f, h, IA_JCC_Z);
    ungen_swartch(cs, f, h, &cstate->u.pattern.facts);
  } break;
  default:
    UNREACHABLE();
  }
}

/* chase mark */
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
    /* TODO: We could also get this information from some var info or
    something.  We could save the cost of copying the
    ast_typeexpr. */
    struct ast_typeexpr *var_type = ast_var_statement_type(&s->u.var_statement);
    uint32_t var_size = gp_sizeof(&cs->nt, var_type);
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
    vardata_init(&vd, s->u.var_statement.decl.name.value,
                 1,
                 ast_var_statement_type(&s->u.var_statement),
                 var_loc);
    SLICE_PUSH(h->vardata, h->vardata_count, h->vardata_limit, vd);
    (*vars_pushed_ref)++;
  } break;
  case AST_STATEMENT_IFTHEN: {
    int32_t saved_offset = frame_save_offset(h);
    struct condition_state cstate;
    if (!gen_condition(cs, f, h, &s->u.ifthen_statement.condition, &cstate)) {
      return 0;
    }

    size_t target_number = frame_add_target(h);
    size_t end_target_number = frame_add_target(h);
    if (!gen_successbody(cs, f, h, &cstate, &s->u.ifthen_statement.body,
                         saved_offset, target_number, end_target_number)) {
      return 0;
    }

    frame_define_target(h, target_number,
                        objfile_section_size(objfile_text(f)));
    gen_afterfail_condition_cleanup(cs, f, h, &cstate);
    frame_define_target(h, end_target_number,
                        objfile_section_size(objfile_text(f)));
    pop_cstate_vardata(h, &cstate);
    frame_restore_offset(h, saved_offset);
  } break;
  case AST_STATEMENT_IFTHENELSE: {
    int32_t saved_offset = frame_save_offset(h);
    struct condition_state cstate;
    if (!gen_condition(cs, f, h, &s->u.ifthenelse_statement.condition, &cstate)) {
      return 0;
    }

    size_t target_number = frame_add_target(h);
    size_t end_target_number = frame_add_target(h);
    if (!gen_successbody(cs, f, h, &cstate, &s->u.ifthenelse_statement.thenbody,
                         saved_offset, target_number, end_target_number)) {
      return 0;
    }

    frame_define_target(h, target_number, objfile_section_size(objfile_text(f)));

    if (!gen_bracebody(cs, f, h, &s->u.ifthenelse_statement.elsebody)) {
      return 0;
    }

    gen_afterfail_condition_cleanup(cs, f, h, &cstate);
    frame_define_target(h, end_target_number,
                        objfile_section_size(objfile_text(f)));
    pop_cstate_vardata(h, &cstate);
    frame_restore_offset(h, saved_offset);
  } break;
  case AST_STATEMENT_WHILE: {
    size_t top_target_number = frame_add_target(h);
    frame_define_target(h, top_target_number, objfile_section_size(objfile_text(f)));

    int32_t saved_offset = frame_save_offset(h);
    struct condition_state cstate;
    if (!gen_condition(cs, f, h, &s->u.while_statement.condition, &cstate)) {
      return 0;
    }

    size_t bottom_target_number = frame_add_target(h);
    size_t end_target_number = frame_add_target(h);
    if (!gen_successbody(cs, f, h, &cstate, &s->u.while_statement.body,
                         saved_offset, bottom_target_number, top_target_number)) {
      return 0;
    }

    frame_define_target(h, bottom_target_number,
                        objfile_section_size(objfile_text(f)));
    gen_afterfail_condition_cleanup(cs, f, h, &cstate);
    frame_define_target(h, end_target_number,
                        objfile_section_size(objfile_text(f)));
    pop_cstate_vardata(h, &cstate);
    frame_restore_offset(h, saved_offset);
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
      frame_define_target(h, bottom_target_number,
                          objfile_section_size(objfile_text(f)));
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

    struct swartch_facts facts;
    if (!gen_swartch(cs, f, h, ss->swartch, &facts)) {
      return 0;
    }

    struct loc swartch_num_loc = make_enum_num_loc(f, h, facts.enum_loc);

    gp_gen_load_register(f, GP_A, swartch_num_loc);

    size_t end_target = frame_add_target(h);
    size_t next_target = frame_add_target(h);

    int has_default = 0;
    size_t default_case_num = 0;

    for (size_t i = 0, e = ss->cased_statements_count; i < e; i++) {
      int32_t switchcase_saved_offset = frame_save_offset(h);
      struct ast_cased_statement *cas = &ss->cased_statements[i];

      if (cas->pattern.is_default) {
        CHECK(!has_default);
        has_default = 1;
        default_case_num = i;
        continue;
      }

      struct ast_constructor_pattern *constructor = &cas->pattern.u.constructor;

      if (!gen_casebody(cs, f, h, &facts, constructor, &cas->body,
                        next_target)) {
        return 0;
      }

      gen_placeholder_jmp(f, h, end_target);
      frame_define_target(h, next_target, objfile_section_size(objfile_text(f)));
      next_target = frame_add_target(h);
      frame_restore_offset(h, switchcase_saved_offset);
    }

    if (has_default) {
      struct ast_cased_statement *cas = &ss->cased_statements[default_case_num];
      /* TODO(): Enum tag size presumption. */
      STATIC_CHECK(FIRST_ENUM_TAG_NUMBER == 1);
      /* We carefully make the 0 tag and nonsense tag values redirect
      to the crash branch. */
      gp_gen_mov_reg_imm32(f, GP_C, FIRST_ENUM_TAG_NUMBER);
      ia_gen_sub(f, GP_A, GP_C, OZ_32);
      CHECK(4 == enum_tag_size(cs->arch));
      ia_gen_cmp_imm(f, GP_A,
                     size_to_int32(ast_case_pattern_info_constructor_number(&cas->pattern.u.default_pattern.info).value),
                     OZ_32);
      gen_placeholder_jcc(f, h, IA_JCC_AE, next_target);

      if (!gen_bracebody(cs, f, h,
                         &ss->cased_statements[default_case_num].body)) {
        return 0;
      }

      gen_placeholder_jmp(f, h, end_target);
      frame_define_target(h, next_target, objfile_section_size(objfile_text(f)));
    }

    gen_crash_jmp(f, h);

    frame_define_target(h, end_target, objfile_section_size(objfile_text(f)));
    ungen_swartch(cs, f, h, &facts);

    /* Pop swartch vardata. */
    SLICE_POP(h->vardata, h->vardata_count, vardata_destroy);
    frame_restore_offset(h, saved_offset);
  } break;
  default:
    UNREACHABLE();
  }

  return 1;
}

/* chase mark */
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

/* chase mark */
int gen_lambda_expr(struct checkstate *cs, struct objfile *f,
                    struct ast_expr *a) {
  CHECK(a->tag == AST_EXPR_LAMBDA);
  /* This code is much like build_typetrav_defs. */
  struct frame h;
  frame_init(&h, cs->platform);

  gen_function_intro(f, &h);
  note_param_locations(cs, f, &h, a);

  int res = gen_bracebody(cs, f, &h, &a->u.lambda.body);

  if (res) {
    gen_function_exit(cs, f, &h);
    tie_jmps(f, &h);
    tie_stack_adjustments(f, &h);
  }

  frame_destroy(&h);
  return res;
}

/* chase mark */
int build_instantiation(struct checkstate *cs, struct objfile *f,
                        struct def_instantiation *inst) {
  struct static_value *value = di_value(inst);
  switch (value->tag) {
  case STATIC_VALUE_U64: {
    STATIC_CHECK(sizeof(value->u.u64_value) == 8);
    /* y86/x64 */
    objfile_section_align_quadword(objfile_data(f));
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    char buf[8];
    write_le_u64(buf, value->u.u64_value);
    objfile_section_append_raw(objfile_data(f), buf, sizeof(buf));
    return 1;
  } break;
  case STATIC_VALUE_U32: {
    STATIC_CHECK(sizeof(value->u.u32_value) == 4);
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    char buf[4];
    write_le_u32(buf, value->u.u32_value);
    objfile_section_append_raw(objfile_data(f), buf, sizeof(buf));
    return 1;
  } break;
  case STATIC_VALUE_I32: {
    STATIC_CHECK(sizeof(value->u.i32_value) == 4);
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    char buf[4];
    write_le_i32(buf, value->u.i32_value);
    objfile_section_append_raw(objfile_data(f), buf, sizeof(buf));
    return 1;
  } break;
  case STATIC_VALUE_U8: {
    /* TODO: Things break if global bytes are aligned to 1 byte, even
    though it should be OK. */
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    uint8_t bytes[4] = { 0 };
    bytes[0] = value->u.u8_value;
    objfile_section_append_raw(objfile_data(f), bytes, 4);
    return 1;
  } break;
  case STATIC_VALUE_BOOL: {
    /* TODO: Things break if global bytes are aligned to 1 byte, even
    though it should be OK. */
    objfile_section_align_dword(objfile_data(f));
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    uint8_t bytes[4] = { 0 };
    CHECK(value->u.bool_value == 0 || value->u.bool_value == 1);
    bytes[0] = (uint8_t)value->u.bool_value;
    objfile_section_append_raw(objfile_data(f), bytes, 4);
    return 1;
  } break;
  case STATIC_VALUE_ENUMVOID: {
    /* We just align to max alignment instead of computing alignof.
    As long as enum tags are 4 or 8 bytes, we could use exact alignof,
    I think -- but see comments above about global byte alignment
    having to be... extra. */
    objfile_section_align(objfile_data(f), max_possible_alignof(cs->arch));
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
                             objfile_section_size(objfile_data(f)));
    /* TODO(): well crap, this is tied to enum representation. */
    char buf[4];
    write_le_u32(buf, size_to_uint32(size_add(value->u.enumvoid_value.enumconstruct_number, FIRST_ENUM_TAG_NUMBER)));
    objfile_section_append_raw(objfile_data(f), buf, sizeof(buf));
    objfile_section_append_zeros(objfile_data(f),
                                 size_sub(value->u.enumvoid_value.enumsize, 4));
    return 1;
  } break;
  case STATIC_VALUE_LAMBDA: {
    /* y86/x64: 16 byte function pointer alignment. */
    /* TODO(): Verify this on Linux, Windows. */
    objfile_fillercode_align_double_quadword(f);
    objfile_set_symbol_value(f, di_symbol_table_index(inst),
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

/* chase mark */
void build_typetrav_defs(struct checkstate *cs,
                         struct objfile *f) {
  /* cs.typetrav_symbol_infos_count is a moving target -- we see more
  typetravs to generate when we generate the first. */
  while (cs->typetrav_symbol_infos_first_ungenerated < cs->typetrav_symbol_infos_count) {
    struct typetrav_symbol_info *info = cs->typetrav_symbol_infos[cs->typetrav_symbol_infos_first_ungenerated];
    /* y86/x64: 16 byte function pointer alignment. */
    objfile_fillercode_align_double_quadword(f);
    objfile_set_symbol_value(f, info->symbol_table_index,
                             objfile_section_size(objfile_text(f)));
    /* TODO: frame has vardata, targetdata, jmpdata -- maybe we should
    decouple the raw codegen stuff. */
    struct frame h;
    frame_init(&h, cs->platform);

    gen_function_intro(f, &h);
    {
      /* Done by note_param_locations. */
      struct loc dummy_void_return_loc = frame_push_loc(&h, 0);
      /* Set 0 for arg_count since we don't put them in vardata. */
      frame_specify_calling_info(&h, 0, dummy_void_return_loc, 0);
    }

    int has_src = info->func == TYPETRAV_FUNC_COPY || info->func == TYPETRAV_FUNC_MOVE_OR_COPYDESTROY;
    uint32_t sz = gp_sizeof(&cs->nt, &info->type);

    switch (cs->arch) {
    case TARGET_ARCH_Y86: {
      /* TODO: This duplicates calling-convention-specific logic of
      note_param_locations. */
      /* src is a garbage value if has_src is false. */
      struct loc src = ebp_indirect_loc(sz, sz, 3 * Y86_DWORD_SIZE);
      struct loc dest = ebp_indirect_loc(sz, sz, 2 * Y86_DWORD_SIZE);

      really_gen_typetrav_behavior(cs, f, &h, info->func, dest, has_src, src,
                                   &info->type);
    } break;
    case TARGET_ARCH_X64: {
      struct loc destptr = frame_push_loc(&h, X64_EIGHTBYTE_SIZE);
      x64_gen_store_register(f, destptr, x64_param_regs[0], X64_RAX);
      struct loc dest = ebp_indirect_loc(sz, sz, destptr.u.ebp_offset);

      struct loc src;
      if (has_src) {
        struct loc srcptr = frame_push_loc(&h, X64_EIGHTBYTE_SIZE);
        x64_gen_store_register(f, srcptr, x64_param_regs[1], X64_RAX);
        src = ebp_indirect_loc(sz, sz, srcptr.u.ebp_offset);
      } else {
        src.tag = (enum loc_tag)-1;
      }
      really_gen_typetrav_behavior(cs, f, &h, info->func, dest, has_src, src,
                                   &info->type);
    } break;
    default:
      UNREACHABLE();
    }

    gen_function_exit(cs, f, &h);
    tie_jmps(f, &h);
    tie_stack_adjustments(f, &h);

    frame_destroy(&h);

    cs->typetrav_symbol_infos_first_ungenerated++;
  }
}

const char *platform_objfile_suffix(enum target_platform platform) {
  switch (platform) {
  case TARGET_PLATFORM_WIN_32BIT:
    return ".obj";
  case TARGET_PLATFORM_LINUX_32BIT: /* fallthrough */
  case TARGET_PLATFORM_OSX_32BIT: /* fallthrough */
  case TARGET_PLATFORM_OSX_64BIT: /* fallthrough */
  case TARGET_PLATFORM_LINUX_64BIT:
    return ".o";
  default:
    UNREACHABLE();
  }
}

int build_module(struct identmap *im,
                 enum target_platform platform,
                 void *loader_ctx,
                 module_loader *loader,
                 ident_value name) {
  int ret = 0;
  struct checkstate cs;
  checkstate_init(&cs, im, loader_ctx, loader, platform);

  if (!chase_modules_and_typecheck(&cs, name)) {
    DBG("(Fail.)\n");
    goto cleanup_checkstate;
  }

  struct objfile *objfile = NULL;
  objfile_alloc(&objfile, platform);

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

  build_typetrav_defs(&cs, objfile);

  struct databuf *databuf = NULL;
  switch (platform) {
  case TARGET_PLATFORM_WIN_32BIT:
    win_flatten(cs.im, objfile, &databuf);
    break;
  case TARGET_PLATFORM_LINUX_32BIT:
    linux32_flatten(cs.im, objfile, &databuf);
    break;
  case TARGET_PLATFORM_OSX_32BIT:
    osx32_flatten(cs.im, objfile, &databuf);
    break;
  case TARGET_PLATFORM_OSX_64BIT:
    TODO_IMPLEMENT;
    break;
  case TARGET_PLATFORM_LINUX_64BIT:
    linux64_flatten(cs.im, objfile, &databuf);
    break;
  default:
    UNREACHABLE();
  }

  void *buf;
  size_t buf_size;
  databuf_move_destroy(databuf, &buf, &buf_size);
  free(databuf);

  const void *name_buf;
  size_t name_count;
  identmap_lookup(im, name, &name_buf, &name_count);
  char *path;
  size_t path_count;
  alloc_half_strcat(name_buf, name_count, platform_objfile_suffix(platform),
                    &path, &path_count);
  if (!write_file(path, buf, buf_size)) {
    ERR("Could not write object file\n");
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
