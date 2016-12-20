#include "arch_ia.h"

#include "objfile_objfile.h"
#include "util.h"

enum x64_reg map_x64_reg(enum gp_reg reg) {
  return (enum x64_reg)reg;
}

enum x86_reg map_x86_reg(enum gp_reg reg) {
  return (enum x86_reg)reg;
}

enum gp_reg unmap_x86_reg(enum x86_reg reg) {
  return (enum gp_reg)reg;
}

enum gp_reg unmap_x64_reg(enum x64_reg reg) {
  CHECK(reg <= X64_RDI);
  return (enum gp_reg)reg;
}

/* Returns true if the reg has a lobyte (also, the register code
happens to identify the lowbyte in a reg or modr/m field). */
int x86_reg_has_lowbyte(enum x86_reg reg) {
  return reg == X86_EAX || reg == X86_ECX || reg == X86_EDX || reg == X86_EBX;
}

int x86_reg8_is_lowbyte(enum x86_reg8 reg) {
  return reg == X86_AL || reg == X86_CL || reg == X86_DL || reg == X86_BL;
}

enum x86_reg8 lowbytereg(enum x86_reg reg) {
  CHECK(x86_reg_has_lowbyte(reg));
  return (enum x86_reg8)reg;
}

enum x86_reg16 map_x86_reg16(enum gp_reg reg) {
  return (enum x86_reg16)reg;
}

enum x86_reg8 map_x86_reg8(enum gp_reg reg) {
  return lowbytereg(map_x86_reg(reg));
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
  CHECK(opnum & 1);
  if (oz == OZ_8) {
    pushtext(f, opnum ^ 1);
    return;
  }
  ia_prefix_no_oz8(f, opnum, oz);
}

/* "reg" specifies the reg field of a modr/m byte, i.e. REX.R. */
/* "base" specifies the r/m field or SIB base or what-have-you.
Beware that oz is assumed to refer to both register sizes!  (But one
might really be a pointer.)  */
/* Just pass in X64_RAX if the reg or base aren't supposed to affect
the REX prefix. */
void x64_super_prefix(struct objfile *f, uint8_t opnum, enum oz oz,
                      enum x64_reg reg,
                      int *regnum_out,
                      enum x64_reg base,
                      int *basenum_out) {
  CHECK(opnum & 1);
  *regnum_out = reg & 7;
  *basenum_out = base & 7;
  if (oz == OZ_8) {
    if (reg > X64_RBX || base > X64_RBX) {
      pushtext(f, (reg <= X64_RDI ? kREX : kREXR) | (base <= X64_RDI ? kREX : kREXB));
    }
    pushtext(f, opnum ^ 1);
    return;
  }
  uint8_t rex = (reg > X64_RDI ? kREXR : 0)
    | (oz == OZ_64 ? kREXW : 0)
    | (base > X64_RDI ? kREXB : 0);
  if (rex) {
    pushtext(f, rex);
  }
  if (oz == OZ_16) {
    pushtext(f, 0x66);
  }
  pushtext(f, opnum);
}

void x64_prefix(struct objfile *f, uint8_t opnum, enum oz oz,
                enum x64_reg reg,
                int *regnum_out) {
  int basenum;
  x64_super_prefix(f, opnum, oz, reg, regnum_out, X64_RAX, &basenum);
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

/* Check callers if max count returned increases. */
size_t x86_encode_reg_rm(uint8_t *b, int reg, int rm_addr,
                         int32_t rm_addr_disp) {
  CHECK(reg <= 7 && rm_addr <= 7);
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

uint8_t mod_reg_rm(int mod, int reg, int rm) {
  CHECK(reg <= 7 && mod <= 3 && rm <= 7);
  return (uint8_t)((mod << 6) | (reg << 3) | rm);
}

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

/* oz depicts the source operand -- the dest is always the full
register, which gets zero-extended */
void x64_gen_movzx(struct objfile *f, enum x64_reg dest,
                   enum x64_reg src_addr, int32_t src_disp, enum oz oz) {
  CHECK(src_addr <= X64_RDI);
  int regnum = dest & 7;
  if (oz <= OZ_16) {
    if (dest > X64_RDI || (oz == OZ_8 && dest > X64_RBX)) {
      pushtext(f, (dest > X64_RDI ? kREXR : kREX));
    }
    uint8_t pref[2];
    pref[0] = 0x0F;
    pref[1] = 0xB6 + (oz == OZ_16);
    apptext(f, pref, 2);
  } else {
    int altregnum;
    x64_prefix(f, 0x8B, oz, dest, &altregnum);
    CHECK(altregnum == regnum);
  }

  uint8_t b[9];
  size_t count = x86_encode_reg_rm(b, regnum, src_addr, src_disp);
  CHECK(count <= 9);
  apptext(f, b, count);
}

void x64_gen_store(struct objfile *f, enum x64_reg dest_addr, int32_t dest_disp,
                   enum x64_reg src, enum oz oz) {
  CHECK(dest_addr <= X64_RDI);
  int regnum;
  x64_prefix(f, 0x89, oz, src, &regnum);
  uint8_t b[9];
  size_t count = x86_encode_reg_rm(b, regnum, dest_addr, dest_disp);
  CHECK(count <= 9);
  apptext(f, b, count);
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

/* oz depicts the source operand -- the dest is always the full
register, which gets zero-extended. */
void ia_gen_movzx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz) {
  CHECK((enum x64_reg)src_addr <= X64_RDI);
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

void ia_gen_movsx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz) {
  check_x86x64(f);
  if (src_oz <= OZ_16) {
    uint8_t b[11];
    b[0] = 0x0F;
    b[1] = 0xBF ^ (src_oz == OZ_8);
    size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
    CHECK(count <= 9);
    apptext(f, b, count + 2);
  } else if (src_oz == OZ_32) {
    switch (objfile_arch(f)) {
    case TARGET_ARCH_X86:
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
    ia_gen_movzx(f, dest, src_addr, src_disp, OZ_64);
  }
}

void ia_help_gen_mov_mem_imm32(struct objfile *f,
                               enum gp_reg dest,
                               int32_t dest_disp,
                               char buf[4]) {
  check_x86x64(f);
  uint8_t b[10];
  b[0] = 0xC7;
  size_t count = x86_encode_reg_rm(b + 1, 0, map_x86_reg(dest), dest_disp);
  CHECK(count <= 9);
  apptext(f, b, count + 1);
  apptext(f, buf, 4);
}

void ia_gen_mov_mem_imm8(struct objfile *f,
                         enum gp_reg dest,
                         int32_t dest_disp,
                         int8_t imm) {
  check_x86x64(f);
  uint8_t b[11];
  b[0] = 0xC6;
  size_t count = x86_encode_reg_rm(b + 1, 0, map_x86_reg(dest), dest_disp);
  CHECK(count <= 9);
  b[1 + count] = (uint8_t)imm;
  apptext(f, b, 1 + count + 1);
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

void ia_gen_xor(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x31, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void ia_gen_shl_cl(struct objfile *f, enum gp_reg dest, enum oz oz) {
  ia_prefix(f, 0xD3, oz);
  /* SHL, SHR, SAR have different reg/opcode fields. */
  pushtext(f, mod_reg_rm(MOD11, 4, dest));
}

void x64_gen_shr_imm(struct objfile *f, enum x64_reg dest, int8_t imm, enum oz oz) {
  int regnum;
  int destnum;
  x64_super_prefix(f, 0xC1, oz, X64_RAX, &regnum, dest, &destnum);
  CHECK(regnum == 0);
  pushtext(f, mod_reg_rm(MOD11, 5, destnum));
  pushtext(f, (uint8_t)imm);
}

void x64_gen_shl_imm(struct objfile *f, enum x64_reg dest, int8_t imm, enum oz oz) {
  int regnum;
  int destnum;
  x64_super_prefix(f, 0xC1, oz, X64_RAX, &regnum, dest, &destnum);
  CHECK(regnum == 0);
  pushtext(f, mod_reg_rm(MOD11, 4, destnum));
  pushtext(f, (uint8_t)imm);
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

void ia_gen_or(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x09, oz);
  pushtext(f, mod_reg_rm(MOD11, src, dest));
}

void x64_gen_or(struct objfile *f, enum x64_reg dest, enum x64_reg src, enum oz oz) {
  int regnum;
  int basenum;
  x64_super_prefix(f, 0x09, oz, src, &regnum, dest, &basenum);
  pushtext(f, mod_reg_rm(MOD11, regnum, basenum));
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

void x64_mov_imm64(struct objfile *f, enum x64_reg dest, int64_t imm64) {
  uint8_t b[10];
  b[0] = kREXW | (dest <= X64_RDI ? 0 : kREXB);
  b[1] = 0xB8 + (dest & 7);
  write_le_i64(b + 2, imm64);
  apptext(f, b, 10);
}

void ia_gen_mov_reg_imm32(struct objfile *f, enum gp_reg dest, int32_t imm32) {
  if (imm32 == 0) {
    ia_gen_xor(f, dest, dest, ptr_oz(f));
  } else {
    enum oz oz = ptr_oz(f);
    if (oz <= OZ_32 || imm32 >= 0) {
      CHECK(dest <= GP_DI);
      ia_prefix_no_oz8(f, 0xB8 + (uint8_t)dest, OZ_32);
      uint8_t b[4];
      write_le_i32(b, imm32);
      apptext(f, b, 4);
    } else {
      CHECK(objfile_arch(f) == TARGET_ARCH_X64);
      x64_mov_imm64(f, map_x64_reg(dest), imm32);
    }
  }
}

void ia_gen_store(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                  enum gp_reg src, enum oz oz) {
  ia_prefix(f, 0x89, oz);
  uint8_t b[9];
  size_t count = x86_encode_reg_rm(b, src, dest_addr, dest_disp);
  CHECK(count <= 9);
  apptext(f, b, count);
}

void ia_gen_call(struct objfile *f, struct sti func_sti) {
  uint8_t b = 0xE8;
  apptext(f, &b, 1);
  objfile_section_append_rel32(objfile_text(f), func_sti);
}

void ia_gen_indirect_call_reg(struct objfile *f, enum gp_reg reg) {
  check_x86x64(f);
  uint8_t b[2];
  b[0] = 0xFF;
  b[1] = mod_reg_rm(MOD11, 2, reg);
  apptext(f, b, 2);
}
