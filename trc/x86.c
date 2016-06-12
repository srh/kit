#include "x86.h"

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

void x64_gen_load64(struct objfile *f, enum x64_reg dest, enum x64_reg src_addr,
                    int32_t src_disp) {
  uint8_t b[11];
  b[0] = kREXW;
  b[1] = 0x8B;
  size_t count = x86_encode_reg_rm(b + 2, dest, src_addr, src_disp);
  CHECK(count <= 9);
  apptext(f, b, count + 2);
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

