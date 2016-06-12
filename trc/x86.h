#ifndef KIT_X86_H_
#define KIT_X86_H_

#include <stdint.h>

#include "gp.h"

struct objfile;
struct sti;

/* Obviously some stuff is named x86_, some is named x64_, others are
named ia_ because they work on both intel architecture platforms. */

#define Y86_DWORD_SIZE 4

#define X64_EIGHTBYTE_SIZE 8

/* REX.W */
#define kREXW 0x48

#define MOD00 0
#define MOD01 1
#define MOD10 2
#define MOD11 3

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

void ia_prefix_no_oz8(struct objfile *f, uint8_t opnum, enum oz oz);
void ia_prefix(struct objfile *f, uint8_t opnum, enum oz oz);
void ia_imm(struct objfile *f, int32_t imm, enum oz oz);

size_t x86_encode_reg_rm(uint8_t *b, int reg, int rm_addr,
                         int32_t rm_addr_disp);

uint8_t mod_reg_rm(int mod, int reg, int rm);
void ia_gen_push(struct objfile *f, enum gp_reg reg);
void ia_gen_pop(struct objfile *f, enum gp_reg reg);
void ia_gen_ret(struct objfile *f);
void x86_gen_retn(struct objfile *f, uint16_t imm16);
void ia_gen_mov(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);

void x64_gen_load64(struct objfile *f, enum x64_reg dest, enum x64_reg src_addr,
                    int32_t src_disp);
void x64_gen_store64(struct objfile *f, enum x64_reg dest_addr, int32_t dest_disp,
                     enum x64_reg src);
void ia_gen_movzx8_reg8(struct objfile *f, enum gp_reg dest, enum x86_reg8 src);
void ia_gen_lea(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                int32_t src_disp);
void ia_gen_movzx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz);
void ia_gen_movsx(struct objfile *f, enum gp_reg dest, enum gp_reg src_addr,
                  int32_t src_disp, enum oz src_oz);
void ia_help_gen_mov_mem_imm32(struct objfile *f,
                               enum gp_reg dest,
                               int32_t dest_disp,
                               char buf[4]);
void ia_gen_mov_mem_imm8(struct objfile *f,
                         enum gp_reg dest,
                         int32_t dest_disp,
                         int8_t imm);
void ia_gen_mov_reg8(struct objfile *f, enum x86_reg8 dest, enum x86_reg8 src);
void ia_gen_test_regs(struct objfile *f, enum gp_reg reg1, enum gp_reg reg2, enum oz oz);
void ia_gen_xor(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);
void ia_gen_shl_cl(struct objfile *f, enum gp_reg dest, enum oz oz);
void ia_gen_shr_cl(struct objfile *f, enum gp_reg dest, enum oz oz);
void ia_gen_sar_cl(struct objfile *f, enum gp_reg dest, enum oz oz);
void x86_gen_add_esp_i32(struct objfile *f, int32_t x);
void ia_gen_add(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);
void ia_gen_azdz_mul(struct objfile *f, enum gp_reg src, enum oz oz);
void ia_gen_alah_mul_w8(struct objfile *f, enum x86_reg8 src);
void ia_gen_imul(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);
void ia_gen_alah_imul_w8(struct objfile *f, enum x86_reg8 src);
void ia_gen_azdz_div(struct objfile *f, enum gp_reg denom, enum oz oz);
void ia_gen_alah_div_w8(struct objfile *f, enum x86_reg8 denom);
void ia_gen_azdz_idiv(struct objfile *f, enum gp_reg denom, enum oz oz);
void ia_gen_alah_idiv_w8(struct objfile *f, enum x86_reg8 denom);
void ia_gen_cwdqo(struct objfile *f, enum oz oz);
void ia_gen_cmp(struct objfile *f, enum gp_reg lhs, enum gp_reg rhs, enum oz oz);
void ia_gen_cmp_imm(struct objfile *f, enum gp_reg lhs, int32_t imm, enum oz oz);
void ia_gen_or(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);
void ia_gen_and(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);
void ia_gen_not(struct objfile *f, enum gp_reg dest, enum oz oz);
void ia_gen_neg(struct objfile *f, enum gp_reg dest, enum oz oz);
void ia_gen_sub(struct objfile *f, enum gp_reg dest, enum gp_reg src, enum oz oz);
void x64_gen_sub_w64_imm32(struct objfile *f, enum x64_reg dest, int32_t imm);
void ia_gen_setcc_b8(struct objfile *f, enum x86_reg8 dest,
                     enum ia_setcc code);
void ia_gen_mov_reg_imm32(struct objfile *f, enum gp_reg dest, int32_t imm32);
void ia_gen_store(struct objfile *f, enum gp_reg dest_addr, int32_t dest_disp,
                  enum gp_reg src, enum oz oz);
void x64_mov_imm64(struct objfile *f, enum x64_reg dest, int64_t imm64);
void ia_gen_call(struct objfile *f, struct sti func_sti);
void ia_gen_indirect_call_reg(struct objfile *f, enum gp_reg reg);

#endif /* KIT_X86_H_ */
