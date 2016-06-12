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

void apptext(struct objfile *f, const void *buf, size_t count);
void pushtext(struct objfile *f, uint8_t byte);
enum oz ptr_oz(struct objfile *f);
/* A no-op -- f->arch must be one of TARGET_ARCH_Y86 or TARGET_ARCH_X64. */
#define check_y86x64(f) do { } while (0)

#endif /* KIT_GP_H_ */
