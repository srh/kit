#ifndef KIT_GP_H_
#define KIT_GP_H_

#include <stddef.h>
#include <stdint.h>

struct objfile;

/* I'm not sure "gp" really means anything. */

/* These "general purpose" register names mimic the y86 mnemonics (for
now) because they stomp on registers of the same name, and legacy code
uses y86-specific names. */
enum gp_reg {
  /* 3 caller-save registers */
  GP_A,
  GP_C,
  GP_D,

  /* 5 callee-save registers, we don't use any but SP and BP */
  GP_B,
  /* stack pointer */
  GP_SP,
  /* frame pointer */
  GP_BP,
  GP_SI,
  GP_DI,
};

enum oz { OZ_8, OZ_16, OZ_32, OZ_64, };

/* NOTE: Obviously we'll need platform-independent values at some point. */

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

void apptext(struct objfile *f, const void *buf, size_t count);
void pushtext(struct objfile *f, uint8_t byte);
enum oz ptr_oz(struct objfile *f);
/* A no-op -- f->arch must be one of TARGET_ARCH_Y86 or TARGET_ARCH_X64. */
#define check_y86x64(f) do { } while (0)

#endif /* KIT_GP_H_ */
