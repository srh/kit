#include "x86.h"

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

