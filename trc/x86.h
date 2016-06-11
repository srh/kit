#ifndef KIT_X86_H_
#define KIT_X86_H_

#include "gp.h"

#define Y86_DWORD_SIZE 4

#define X64_EIGHTBYTE_SIZE 8

/* REX.W */
#define kREXW 0x48


enum x64_reg {
  X64_RAX,
  X64_RCX,
  X64_RDX,
  X64_RBX,
  X64_RSP,
  X64_RBP,
  X64_RSI,
  X64_RDI,
  X64_R8,
  X64_R9,
  X64_R10,
  X64_R11,
  X64_R12,
  X64_R13,
  X64_R14,
  X64_R15,
};

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

enum x64_reg map_x64_reg(enum gp_reg reg);
enum x86_reg map_x86_reg(enum gp_reg reg);
enum gp_reg unmap_x86_reg(enum x86_reg reg);

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

/* Returns true if the reg has a lobyte (also, the register code
happens to identify the lowbyte in a reg or modr/m field). */
int x86_reg_has_lowbyte(enum x86_reg reg);
int x86_reg8_is_lowbyte(enum x86_reg8 reg);
enum x86_reg8 lowbytereg(enum x86_reg reg);
enum x86_reg16 map_x86_reg16(enum gp_reg reg);
enum x86_reg8 map_x86_reg8(enum gp_reg reg);

#endif /* KIT_X86_H_ */
