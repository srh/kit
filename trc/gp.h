#ifndef KIT_GP_H_
#define KIT_GP_H_

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

#endif /* KIT_GP_H_ */
