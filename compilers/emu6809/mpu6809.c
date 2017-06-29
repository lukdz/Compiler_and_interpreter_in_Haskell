#include "mpu6809.h"

#include <stdio.h>
#include <stdlib.h>

#define FLAG_C 0x01
#define FLAG_V 0x02
#define FLAG_Z 0x04
#define FLAG_N 0x08
#define FLAG_I 0x10
#define FLAG_H 0x20
#define FLAG_F 0x40
#define FLAG_E 0x80

#define FLAGS_ZC    (FLAG_Z | FLAG_C)
#define FLAGS_NZ    (FLAG_N | FLAG_Z)
#define FLAGS_NZV   (FLAG_N | FLAG_Z | FLAG_V)
#define FLAGS_NZC   (FLAG_N | FLAG_Z | FLAG_C)
#define FLAGS_NZVC  (FLAG_N | FLAG_Z | FLAG_V | FLAG_C)
#define FLAGS_HNZVC (FLAG_H | FLAG_N | FLAG_Z | FLAG_V | FLAG_C)

#define MSB(v) ((uint8_t)((v) >> 8))
#define LSB(v) ((uint8_t)(v))
#define WORD(hi,lo) (((uint16_t)(hi) << 8) | (uint16_t)(uint8_t)(lo))

void mpu6809_init(mpu6809 *mpu) {
  mpu->handle_swi  = mpu6809_default_handle_swi;
  mpu->handle_swi2 = mpu6809_default_handle_swi2;
  mpu->handle_swi3 = mpu6809_default_handle_swi3;
}

/* ========================================================================= */
/* Memory operations */

inline static uint8_t read_byte(mpu6809 *mpu, uint16_t addr) {
  return mpu->read_mem(mpu->context, addr);
}

inline static uint16_t read_word(mpu6809 *mpu, uint16_t addr) {
  uint8_t hi = read_byte(mpu, addr);
  uint8_t lo = read_byte(mpu, addr+1);
  return WORD(hi,lo);
}

inline static void write_byte(mpu6809 *mpu, uint16_t addr, uint8_t value) {
  mpu->write_mem(mpu->context, addr, value);
}

inline static void write_word(mpu6809 *mpu, uint16_t addr, uint16_t value) {
  write_byte(mpu, addr,   MSB(value));
  write_byte(mpu, addr+1, LSB(value));
}

inline static uint8_t pop_byte(mpu6809 *mpu, uint16_t *stack) {
  uint8_t value = read_byte(mpu, *stack);
  *stack += 1;
  return value;
}

inline static uint16_t pop_word(mpu6809 *mpu, uint16_t *stack) {
  uint16_t value = read_word(mpu, *stack);
  *stack += 2;
  return value;
}

inline static void push_byte(mpu6809 *mpu, uint16_t *stack, uint8_t value) {
  *stack -= 1;
  write_byte(mpu, *stack, value);
}

inline static void push_word(mpu6809 *mpu, uint16_t *stack, uint16_t value) {
  *stack -= 2;
  write_word(mpu, *stack, value);
}

#define POP_b(s) (pop_byte(mpu, &(mpu->s)))
#define POP_w(s) (pop_word(mpu, &(mpu->s)))
#define PUSH_w(s,v) (push_word(mpu, &(mpu->s), (v)))
#define PUSH_b(s,v) (push_byte(mpu, &(mpu->s), (v)))

#define PSH(s,s2) \
  if (imm8 & 0x80) { PUSH_w(s, mpu->PC); } \
  if (imm8 & 0x40) { PUSH_w(s, mpu->s2); } \
  if (imm8 & 0x20) { PUSH_w(s, mpu->Y);  } \
  if (imm8 & 0x10) { PUSH_w(s, mpu->X);  } \
  if (imm8 & 0x08) { PUSH_b(s, mpu->DP); } \
  if (imm8 & 0x04) { PUSH_b(s, mpu->B);  } \
  if (imm8 & 0x02) { PUSH_b(s, mpu->A);  } \
  if (imm8 & 0x01) { PUSH_b(s, mpu->CC); }

#define PUL(s,s2) \
  if (imm8 & 0x01) { mpu->CC = POP_b(s); } \
  if (imm8 & 0x02) { mpu->A  = POP_b(s); } \
  if (imm8 & 0x04) { mpu->B  = POP_b(s); } \
  if (imm8 & 0x08) { mpu->DP = POP_b(s); } \
  if (imm8 & 0x10) { mpu->X  = POP_w(s); } \
  if (imm8 & 0x20) { mpu->Y  = POP_w(s); } \
  if (imm8 & 0x40) { mpu->s2 = POP_w(s); } \
  if (imm8 & 0x80) { mpu->PC = POP_w(s); }

/* ========================================================================= */
/* 8-bit operations */

#define flag_N8(x)  (((x) & 0x80) ? FLAG_N : 0)
#define flag_Z8(x)  ((uint8_t)(x) ? 0 : FLAG_Z)
#define flag_NZ8(x) (flag_N8(x) | flag_Z8(x))

#define flag_C_value(mpu) ((mpu)->CC & 1)

#define clear_ZC(x)    ((x) & ~FLAGS_ZC)
#define clear_NZ(x)    ((x) & ~FLAGS_NZ)
#define clear_NZV(x)   ((x) & ~FLAGS_NZV)
#define clear_NZC(x)   ((x) & ~FLAGS_NZC)
#define clear_NZVC(x)  ((x) & ~FLAGS_NZVC)
#define clear_HNZVC(x) ((x) & ~FLAGS_HNZVC)

inline static uint8_t add8c(mpu6809 *mpu, uint8_t a, uint8_t b, uint8_t c) {
  uint16_t ae = (int16_t)(int8_t)a;
  uint16_t be = (int16_t)(int8_t)b;
  uint16_t result = ae + be + c;
  mpu->CC = clear_HNZVC(mpu->CC) | flag_NZ8(result)
          | (((result ^ (result >> 1)) & 0x80) ? FLAG_V : 0)
          | (((ae ^ be ^ result) & 0x100) ? FLAG_C : 0)
          | ((((a & 0xF) + (b & 0xF) + c) & 0x10) ? FLAG_H : 0);
  return result;
}

static uint8_t adc8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  return add8c(mpu, a, b, flag_C_value(mpu));
}

static uint8_t add8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  return add8c(mpu, a, b, 0);
}

inline static uint8_t sub8c(mpu6809 *mpu, uint8_t a, uint8_t b, uint8_t c) {
  uint16_t ae = (int16_t)(int8_t)a;
  uint16_t be = (int16_t)(int8_t)b;
  uint16_t result = ae - be - c;
  mpu->CC = clear_NZVC(mpu->CC) | flag_NZ8(result)
          | (((result ^ (result >> 1)) & 0x80) ? FLAG_V : 0)
          | (((ae ^ be ^ result) & 0x100) ? FLAG_C : 0);
  return result;
}

static uint8_t sbc8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  return sub8c(mpu, a, b, flag_C_value(mpu));
}

static uint8_t sub8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  return sub8c(mpu, a, b, 0);
}

static uint8_t and8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  uint8_t result = a & b;
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ8(result);
  return result;
}

static uint8_t or8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  uint8_t result = a | b;
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ8(result);
  return result;
}

static uint8_t eor8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  uint8_t result = a ^ b;
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ8(result);
  return result;
}

static uint8_t ld8(mpu6809 *mpu, uint8_t a, uint8_t b) {
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ8(b);
  return b;
}

static uint8_t neg8(mpu6809 *mpu, uint8_t a) {
  return sub8c(mpu, 0, a, 0);
}

static uint8_t clr8(mpu6809 *mpu, uint8_t a) {
  mpu->CC = clear_NZVC(mpu->CC) | FLAG_Z;
  return 0;
}

static uint8_t com8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = ~a;
  mpu->CC = clear_NZVC(mpu->CC) | flag_NZ8(result) | FLAG_C;
  return result;
}

static uint8_t inc8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = a + 1;
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ8(result)
          | (a == 0x7F ? FLAG_V : 0);
  return result;
}

static uint8_t dec8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = a - 1;
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ8(result)
          | (a == 0x80 ? FLAG_V : 0);
  return result;
}

static uint8_t asr8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = (int16_t)(int8_t)a >> 1;
  mpu->CC = clear_NZC(mpu->CC) | flag_NZ8(result)
          | ((a & 1) ? FLAG_C : 0);
  return result;
}

static uint8_t lsl8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = a << 1;
  mpu->CC = clear_NZVC(mpu->CC) | flag_NZ8(result)
          | (((a ^ result) & 0x80) ? FLAG_V : 0)
          | ((a & 0x80) ? FLAG_C : 0);
  return result;
}

static uint8_t lsr8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = a >> 1;
  mpu->CC = clear_NZC(mpu->CC) | flag_NZ8(result)
          | ((a & 1) ? FLAG_C : 0);
  return result;
}

static uint8_t rol8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = (a << 1) | flag_C_value(mpu);
  mpu->CC = clear_NZVC(mpu->CC) | flag_NZ8(result)
          | (((a ^ result) & 0x80) ? FLAG_V : 0)
          | ((a & 0x80) ? FLAG_C : 0);
  return result;
}

static uint8_t ror8(mpu6809 *mpu, uint8_t a) {
  uint8_t result = (a >> 1) | ((mpu->CC & FLAG_C) ? 0x80 : 0);
  mpu->CC = clear_NZC(mpu->CC) | flag_NZ8(result)
          | ((a & 1) ? FLAG_C : 0);
  return result;
}

static uint8_t tst8(mpu6809 *mpu, uint8_t a) {
  and8(mpu, a, 0xFF);
  return a;
}

static uint8_t daa(mpu6809 *mpu, uint8_t a) {
  uint16_t result = a;
  if (mpu->CC & FLAG_C
    || (result >> 4) > 9
    || ((result >> 4) > 8 && (result & 0xF) > 9))
  {
    result += 6 << 4;
  }
  if (mpu->CC & FLAG_H || (result & 0xF) > 9) {
    result += 6;
  }
  mpu->CC = clear_NZC(mpu->CC) | flag_NZ8(result)
          | ((result & 0x100) ? FLAG_C : 0);
  return result;
}

/* ========================================================================= */
/* 16-bit operations */

#define flag_N16(x)  (((x) & 0x8000) ? FLAG_N : 0)
#define flag_Z16(x)  ((uint16_t)(x) ? 0 : FLAG_Z)
#define flag_NZ16(x) (flag_N16(x) | flag_Z16(x))

inline static uint16_t add16c(mpu6809 *mpu,
    uint16_t a, uint16_t b, uint16_t c)
{
  uint32_t ae = (int16_t)a;
  uint32_t be = (int16_t)b;
  uint32_t result = ae + be + c;
  mpu->CC = clear_NZVC(mpu->CC) | flag_NZ16(result)
          | (((result ^ (result >> 1)) & 0x8000) ? FLAG_V : 0)
          | (((ae ^ be ^ result) & 0x10000) ? FLAG_C : 0);
  return result;
}

static uint16_t add16(mpu6809 *mpu, uint16_t a, uint16_t b) {
  return add16c(mpu, a, b, 0);
}

inline static uint16_t sub16c(mpu6809 *mpu,
    uint16_t a, uint16_t b, uint16_t c)
{
  uint32_t ae = (int16_t)a;
  uint32_t be = (int16_t)b;
  uint32_t result = ae - be - c;
  mpu->CC = clear_NZVC(mpu->CC) | flag_NZ16(result)
          | (((result ^ (result >> 1)) & 0x8000) ? FLAG_V : 0)
          | (((ae ^ be ^ result) & 0x10000) ? FLAG_C : 0);
  return result;
}

static uint16_t sub16(mpu6809 *mpu, uint16_t a, uint16_t b) {
  return sub16c(mpu, a, b, 0);
}

static uint16_t and16(mpu6809 *mpu, uint16_t a, uint16_t b) {
  uint16_t result = a & b;
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ16(result);
  return result;
}

static uint16_t ld16(mpu6809 *mpu, uint16_t a, uint16_t b) {
  mpu->CC = clear_NZV(mpu->CC) | flag_NZ16(b);
  return b;
}

/* ========================================================================= */
/* Other operations */

static uint16_t get_register(mpu6809 *mpu, uint8_t reg) {
  switch (reg) {
  case 0x0: return mpu->D;
  case 0x1: return mpu->X;
  case 0x2: return mpu->Y;
  case 0x3: return mpu->U;
  case 0x4: return mpu->S;
  case 0x5: return mpu->PC;
  case 0x8: return WORD(0xFF, mpu->A);
  case 0x9: return WORD(0xFF, mpu->B);
  case 0xA: return WORD(mpu->CC, mpu->CC);
  case 0xB: return WORD(mpu->DP, mpu->DP);
  default:  return 0xFFFF;
  }
}

static void set_register(mpu6809 *mpu, uint8_t reg, uint16_t value) {
  switch (reg) {
  case 0x0: mpu->D  = value; return;
  case 0x1: mpu->X  = value; return;
  case 0x2: mpu->Y  = value; return;
  case 0x3: mpu->U  = value; return;
  case 0x4: mpu->S  = value; return;
  case 0x5: mpu->PC = value; return;
  case 0x8: mpu->A  = value; return;
  case 0x9: mpu->B  = value; return;
  case 0xA: mpu->CC = value; return;
  case 0xB: mpu->DP = value; return;
  }
}

void mpu6809_default_handle_swi(void *context, mpu6809 *mpu) {
  mpu->CC |= FLAG_E;
  uint8_t imm8 = 0xFF;
  PSH(S,U);
  mpu->CC |= FLAG_I | FLAG_F;
  mpu->PC = read_word(mpu, 0xFFFA);
}

void mpu6809_default_handle_swi2(void *context, mpu6809 *mpu) {
  mpu->CC |= FLAG_E;
  uint8_t imm8 = 0xFF;
  PSH(S,U);
  mpu->PC = read_word(mpu, 0xFFF4);
}

void mpu6809_default_handle_swi3(void *context, mpu6809 *mpu) {
  mpu->CC |= FLAG_E;
  uint8_t imm8 = 0xFF;
  PSH(S,U);
  mpu->PC = read_word(mpu, 0xFFF2);
}

static uint16_t ea_indexed(mpu6809 *mpu) {
  uint8_t mode = POP_b(PC);
  uint16_t *base;
  uint16_t ea;
  switch ((mode >> 5) & 3) {
  case 0: base = &(mpu->X); break;
  case 1: base = &(mpu->Y); break;
  case 2: base = &(mpu->U); break;
  case 3: base = &(mpu->S); break;
  }
  if ((mode & 0x80) == 0) { /* 5-bit offset */
    return *base + ((int16_t)(int8_t)(mode << 3) >> 3);
  }
  switch (mode & 0x1F) {
  case 0x00: ea = *base; *base += 1; return ea;
  case 0x01: ea = *base; *base += 2; return ea;
  case 0x02: *base -= 1; return *base;
  case 0x03: *base -= 2; return *base;
  case 0x04: return *base;
  case 0x05: return *base + (int16_t)(int8_t)mpu->B;
  case 0x06: return *base + (int16_t)(int8_t)mpu->D;
  case 0x08: return *base + (int16_t)(int8_t)POP_b(PC);
  case 0x09: return *base + POP_w(PC);
  case 0x0B: return *base + mpu->D;
  case 0x0C: ea = (int16_t)(int8_t)POP_b(PC); return mpu->PC + ea;
  case 0x0D: ea = POP_w(PC); return mpu->PC + ea;
  case 0x11: ea = *base; *base += 2; return read_word(mpu, ea);
  case 0x13: *base -= 2; return read_word(mpu, *base);
  case 0x14: return read_word(mpu, *base);
  case 0x15: return read_word(mpu, *base + (int16_t)(int8_t)mpu->B);
  case 0x16: return read_word(mpu, *base + (int16_t)(int8_t)mpu->A);
  case 0x18: return read_word(mpu, *base + (int16_t)(int8_t)POP_b(PC));
  case 0x19: return read_word(mpu, *base + POP_w(PC));
  case 0x1B: return read_word(mpu, *base + mpu->D);
  case 0x1C: ea = (int16_t)(int8_t)POP_b(PC);
             return read_word(mpu, mpu->PC + ea);
  case 0x1D: ea = POP_w(PC);         return read_word(mpu, mpu->PC + ea);
  case 0x1F: return read_word(mpu, POP_w(PC));
  default:
    fprintf(stderr, "ILLEGAL INDEXED MODE: %02X\n", (int)mode);
    exit(1);
  }
}

void mpu6809_reset(mpu6809 *mpu) {
  mpu->PC = read_word(mpu, 0xFFFE);
}

/* ========================================================================= */

#define READ_b     (read_byte(mpu, ea))
#define READ_w     (read_word(mpu, ea))
#define WRITE_b(v) (write_byte(mpu, ea, (v)))
#define WRITE_w(v) (write_word(mpu, ea, (v)))

#define MEM_UOP(op)   WRITE_b(op(mpu, READ_b)); return;
#define REG_UOP(r,op) mpu->r = op(mpu, mpu->r); return;
#define TMP_UOP(op)   op(mpu, READ_b); return;
#define TMP_I8(r,op)  op(mpu, mpu->r, imm8); return;
#define TMP_I16(r,op) op(mpu, mpu->r, imm16); return;
#define REG_I8(r,op)  mpu->r = op(mpu, mpu->r, imm8); return;
#define REG_I16(r,op) mpu->r = op(mpu, mpu->r, imm16); return;
#define REG_M8(r,op)  mpu->r = op(mpu, mpu->r, READ_b); return;
#define REG_M16(r,op) mpu->r = op(mpu, mpu->r, READ_w); return;
#define TMP_M8(r,op)  op(mpu, mpu->r, READ_b); return;
#define TMP_M16(r,op) op(mpu, mpu->r, READ_w); return;

#define BR(c)  if (c) { mpu->PC += (int16_t)(int8_t)imm8; } return;
#define BSR  PUSH_w(S, mpu->PC); mpu->PC += (int16_t)(int8_t)imm8; return;

#define LBR(c) if (c) { mpu->PC += imm16; } return;
#define LBSR   PUSH_w(S, mpu->PC); mpu->PC += imm16; return;

#define JMP    mpu->PC = ea; return;
#define JSR    PUSH_w(S, mpu->PC); mpu->PC = ea; return;

#define CLR WRITE_b(clr8(mpu, 0)); return;
#define EXG {\
    /* TODO: implement strange behaviour for different register size */ \
    uint16_t v1 = get_register(mpu, imm8 >> 4); \
    uint16_t v2 = get_register(mpu, imm8 & 0xF); \
    set_register(mpu, imm8 >> 4,  v2); \
    set_register(mpu, imm8 & 0xF, v1); \
  } \
  return;

#define TFR set_register(mpu, imm8 & 0xF, get_register(mpu, imm8 >> 4)); \
        return;

#define LEA_SU(r) mpu->r = ea; return;
#define LEA_XY(r) mpu->r = ea; mpu->CC = (mpu->CC & ~FLAG_Z) | flag_Z16(ea);\
                  return;

#define MUL \
  mpu->D  = (uint16_t)(mpu->A) * (uint16_t)(mpu->B); \
  mpu->CC = clear_ZC(mpu->CC) | flag_Z16(mpu->D) \
          | ((mpu->B & 0x80) ? FLAG_C : 0); \
  return;

#define RTI \
  mpu->CC = POP_b(S); \
  if (mpu->CC & FLAG_E) { \
    imm8 = 0x7E; \
    PUL(S,U) \
  } \
  mpu->PC = POP_w(S); \
  return;

#define SEX \
  mpu->D = (int16_t)(int8_t)(mpu->B); \
  mpu->CC = clear_NZ(mpu->CC) | flag_NZ16(mpu->D); \
  return;

#define ST_8(r)   and8(mpu, mpu->r, 0xFF);    WRITE_b(mpu->r); return;
#define ST_16(r)  and16(mpu, mpu->r, 0xFFFF); WRITE_w(mpu->r); return;

#define COND_CC (mpu->CC & FLAG_C) == 0
#define COND_CS (mpu->CC & FLAG_C) != 0
#define COND_NE (mpu->CC & FLAG_Z) == 0
#define COND_EQ (mpu->CC & FLAG_Z) != 0
#define COND_VC (mpu->CC & FLAG_V) == 0
#define COND_VS (mpu->CC & FLAG_V) != 0
#define COND_PL (mpu->CC & FLAG_N) == 0
#define COND_MI (mpu->CC & FLAG_N) != 0
#define COND_HI COND_NE && COND_CC
#define COND_LS COND_EQ || COND_CS
#define COND_GE ((mpu->CC ^ (mpu->CC >> 2)) & 0x02) == 0
#define COND_LT ((mpu->CC ^ (mpu->CC >> 2)) & 0x02) != 0
#define COND_GT COND_GE && COND_NE
#define COND_LE COND_LT || COND_EQ

#define IMM8    imm8  = POP_b(PC);
#define IMM16   imm16 = POP_w(PC);
#define DIRECT  ea    = WORD(mpu->DP, POP_b(PC));
#define INDEXED ea    = ea_indexed(mpu);
#define EXTENDD ea    = POP_w(PC);

static void prefix_10(mpu6809 *mpu);
static void prefix_11(mpu6809 *mpu);

void mpu6809_step(mpu6809 *mpu) {
  uint8_t  imm8;
  uint16_t imm16;
  uint16_t ea;
  switch (POP_b(PC)) {
  case 0x00:       DIRECT  MEM_UOP(neg8)                 /* NEG   */
  case 0x03:       DIRECT  MEM_UOP(com8)                 /* COM   */
  case 0x04:       DIRECT  MEM_UOP(lsr8)                 /* LSR   */
  case 0x06:       DIRECT  MEM_UOP(ror8)                 /* ROR   */
  case 0x07:       DIRECT  MEM_UOP(asr8)                 /* ASR   */
  case 0x08:       DIRECT  MEM_UOP(lsl8)                 /* LSL   */
  case 0x09:       DIRECT  MEM_UOP(rol8)                 /* ROL   */
  case 0x0A:       DIRECT  MEM_UOP(dec8)                 /* DEC   */
  case 0x0C:       DIRECT  MEM_UOP(inc8)                 /* INC   */
  case 0x0D:       DIRECT  TMP_UOP(tst8)                 /* TST   */
  case 0x0E:       DIRECT  JMP                           /* JMP   */
  case 0x0F:       DIRECT  CLR                           /* CLR   */

  case 0x10: prefix_10(mpu); return;
  case 0x11: prefix_11(mpu); return;
  case 0x12: return;                                     /* NOP   */
  case 0x13: /* TODO */    return;                       /* SYNC  */
  case 0x16: IMM16         LBR(1)                        /* LBRA  */
  case 0x17: IMM16         LBSR                          /* LBSR  */
  case 0x19: mpu->A = daa(mpu, mpu->A); return;          /* DAA   */
  case 0x1A: IMM8 mpu->CC |= imm8; return;               /* ORCC  */
  case 0x1C: IMM8 mpu->CC &= imm8; return;               /* ANDCC */
  case 0x1D:               SEX                           /* SEX   */
  case 0x1E: IMM8          EXG                           /* EXG   */
  case 0x1F: IMM8          TFR                           /* TFR   */

  case 0x20: IMM8          BR(1)                         /* BRA   */
  case 0x21: IMM8          BR(0)                         /* BRN   */
  case 0x22: IMM8          BR(COND_HI)                   /* BHI   */
  case 0x23: IMM8          BR(COND_LS)                   /* BLS   */
  case 0x24: IMM8          BR(COND_CC)                   /* BCC   */
  case 0x25: IMM8          BR(COND_CS)                   /* BCS   */
  case 0x26: IMM8          BR(COND_NE)                   /* BNE   */
  case 0x27: IMM8          BR(COND_EQ)                   /* BEQ   */
  case 0x28: IMM8          BR(COND_VC)                   /* BVC   */
  case 0x29: IMM8          BR(COND_VS)                   /* BVS   */
  case 0x2A: IMM8          BR(COND_PL)                   /* BPL   */
  case 0x2B: IMM8          BR(COND_MI)                   /* BMI   */
  case 0x2C: IMM8          BR(COND_GE)                   /* BGE   */
  case 0x2D: IMM8          BR(COND_LT)                   /* BLT   */
  case 0x2E: IMM8          BR(COND_GT)                   /* BGT   */
  case 0x2F: IMM8          BR(COND_LE)                   /* BLE   */

  case 0x30:       INDEXED LEA_XY(X)                     /* LEAX  */
  case 0x31:       INDEXED LEA_XY(Y)                     /* LEAY  */
  case 0x32:       INDEXED LEA_XY(S)                     /* LEAS  */
  case 0x33:       INDEXED LEA_SU(U)                     /* LEAU  */
  case 0x34: IMM8          PSH(S,U); return;             /* PSHS  */
  case 0x35: IMM8          PUL(S,U); return;             /* PULS  */
  case 0x36: IMM8          PSH(U,S); return;             /* PSHU  */
  case 0x37: IMM8          PSH(U,S); return;             /* PULU  */
  case 0x39: mpu->PC = POP_w(S); return;                 /* RTS   */
  case 0x3A: mpu->X += mpu->B;   return;                 /* ABX   */
  case 0x3B:               RTI                           /* RTI   */
  case 0x3C: IMM8 /* TODO */ return;                     /* CWAI  */
  case 0x3D:               MUL                           /* MUL   */
  case 0x3F: mpu->handle_swi(mpu->context, mpu); return; /* SWI   */

  case 0x40:               REG_UOP(A, neg8)              /* NEGA  */
  case 0x43:               REG_UOP(A, com8)              /* COMA  */
  case 0x44:               REG_UOP(A, lsr8)              /* LSRA  */
  case 0x46:               REG_UOP(A, ror8)              /* RORA  */
  case 0x47:               REG_UOP(A, asr8)              /* ASRA  */
  case 0x48:               REG_UOP(A, lsl8)              /* LSLA  */
  case 0x49:               REG_UOP(A, rol8)              /* ROLA  */
  case 0x4A:               REG_UOP(A, dec8)              /* DECA  */
  case 0x4C:               REG_UOP(A, inc8)              /* INCA  */
  case 0x4D:               REG_UOP(A, tst8)              /* TSTA  */
  case 0x4F:               REG_UOP(A, clr8)              /* CLRA  */

  case 0x50:               REG_UOP(B, neg8)              /* NEGB  */
  case 0x53:               REG_UOP(B, com8)              /* COMB  */
  case 0x54:               REG_UOP(B, lsr8)              /* LSRB  */
  case 0x56:               REG_UOP(B, ror8)              /* RORB  */
  case 0x57:               REG_UOP(B, asr8)              /* ASRB  */
  case 0x58:               REG_UOP(B, lsl8)              /* LSLB  */
  case 0x59:               REG_UOP(B, rol8)              /* ROLB  */
  case 0x5A:               REG_UOP(B, dec8)              /* DECB  */
  case 0x5C:               REG_UOP(B, inc8)              /* INCB  */
  case 0x5D:               REG_UOP(B, tst8)              /* TSTB  */
  case 0x5F:               REG_UOP(B, clr8)              /* CLRB  */

  case 0x60:       INDEXED MEM_UOP(neg8)                 /* NEG   */
  case 0x63:       INDEXED MEM_UOP(com8)                 /* COM   */
  case 0x64:       INDEXED MEM_UOP(lsr8)                 /* LSR   */
  case 0x66:       INDEXED MEM_UOP(ror8)                 /* ROR   */
  case 0x67:       INDEXED MEM_UOP(asr8)                 /* ASR   */
  case 0x68:       INDEXED MEM_UOP(lsl8)                 /* LSL   */
  case 0x69:       INDEXED MEM_UOP(rol8)                 /* ROL   */
  case 0x6A:       INDEXED MEM_UOP(dec8)                 /* DEC   */
  case 0x6C:       INDEXED MEM_UOP(inc8)                 /* INC   */
  case 0x6D:       INDEXED TMP_UOP(tst8)                 /* TST   */
  case 0x6E:       INDEXED JMP                           /* JMP   */
  case 0x6F:       INDEXED CLR                           /* CLR   */

  case 0x70:       EXTENDD MEM_UOP(neg8)                 /* NEG   */
  case 0x73:       EXTENDD MEM_UOP(com8)                 /* COM   */
  case 0x74:       EXTENDD MEM_UOP(lsr8)                 /* LSR   */
  case 0x76:       EXTENDD MEM_UOP(ror8)                 /* ROR   */
  case 0x77:       EXTENDD MEM_UOP(asr8)                 /* ASR   */
  case 0x78:       EXTENDD MEM_UOP(lsl8)                 /* LSL   */
  case 0x79:       EXTENDD MEM_UOP(rol8)                 /* ROL   */
  case 0x7A:       EXTENDD MEM_UOP(dec8)                 /* DEC   */
  case 0x7C:       EXTENDD MEM_UOP(inc8)                 /* INC   */
  case 0x7D:       EXTENDD TMP_UOP(tst8)                 /* TST   */
  case 0x7E:       EXTENDD JMP                           /* JMP   */
  case 0x7F:       EXTENDD CLR                           /* CLR   */

  case 0x80: IMM8          REG_I8(A, sub8)               /* SUBA  */
  case 0x81: IMM8          TMP_I8(A, sub8)               /* CMPA  */
  case 0x82: IMM8          REG_I8(A, sbc8)               /* SBCA  */
  case 0x83: IMM16         REG_I16(D, sub16)             /* SUBD  */
  case 0x84: IMM8          REG_I8(A, and8)               /* ANDA  */
  case 0x85: IMM8          TMP_I8(A, and8)               /* BITA  */
  case 0x86: IMM8          REG_I8(A, ld8)                /* LDA   */
  case 0x88: IMM8          REG_I8(A, eor8)               /* EORA  */
  case 0x89: IMM8          REG_I8(A, adc8)               /* ADCA  */
  case 0x8A: IMM8          REG_I8(A, or8)                /* ORA   */
  case 0x8B: IMM8          REG_I8(A, add8)               /* ADDA  */
  case 0x8C: IMM16         TMP_I16(X, sub16)             /* CMPX  */
  case 0x8D: IMM8          BSR                           /* BSR   */
  case 0x8E: IMM16         REG_I16(X, ld16)              /* LDX   */

  case 0x90:       DIRECT  REG_M8(A, sub8)               /* SUBA  */
  case 0x91:       DIRECT  TMP_M8(A, sub8)               /* CMPA  */
  case 0x92:       DIRECT  REG_M8(A, sbc8)               /* SBCA  */
  case 0x93:       DIRECT  REG_M16(D, sub16)             /* SUBD  */
  case 0x94:       DIRECT  REG_M8(A, and8)               /* ANDA  */
  case 0x95:       DIRECT  TMP_M8(A, and8)               /* BITA  */
  case 0x96:       DIRECT  REG_M8(A, ld8)                /* LDA   */
  case 0x97:       DIRECT  ST_8(A)                       /* STA   */
  case 0x98:       DIRECT  REG_M8(A, eor8)               /* EORA  */
  case 0x99:       DIRECT  REG_M8(A, adc8)               /* ADCA  */
  case 0x9A:       DIRECT  REG_M8(A, or8)                /* ORA   */
  case 0x9B:       DIRECT  REG_M8(A, add8)               /* ADDA  */
  case 0x9C:       DIRECT  TMP_M16(X, sub16)             /* CMPX  */
  case 0x9D:       DIRECT  JSR                           /* JSR   */
  case 0x9E:       DIRECT  REG_M16(X, ld16)              /* LDX   */
  case 0x9F:       DIRECT  ST_16(X)                      /* STX   */

  case 0xA0:       INDEXED REG_M8(A, sub8)               /* SUBA  */
  case 0xA1:       INDEXED TMP_M8(A, sub8)               /* CMPA  */
  case 0xA2:       INDEXED REG_M8(A, sbc8)               /* SBCA  */
  case 0xA3:       INDEXED REG_M16(D, sub16)             /* SUBD  */
  case 0xA4:       INDEXED REG_M8(A, and8)               /* ANDA  */
  case 0xA5:       INDEXED TMP_M8(A, and8)               /* BITA  */
  case 0xA6:       INDEXED REG_M8(A, ld8)                /* LDA   */
  case 0xA7:       INDEXED ST_8(A)                       /* STA   */
  case 0xA8:       INDEXED REG_M8(A, eor8)               /* EORA  */
  case 0xA9:       INDEXED REG_M8(A, adc8)               /* ADCA  */
  case 0xAA:       INDEXED REG_M8(A, or8)                /* ORA   */
  case 0xAB:       INDEXED REG_M8(A, add8)               /* ADDA  */
  case 0xAC:       INDEXED TMP_M16(X, sub16)             /* CMPX  */
  case 0xAD:       INDEXED JSR                           /* JSR   */
  case 0xAE:       INDEXED REG_M16(X, ld16)              /* LDX   */
  case 0xAF:       INDEXED ST_16(X)                      /* STX   */

  case 0xB0:       EXTENDD REG_M8(A, sub8)               /* SUBA  */
  case 0xB1:       EXTENDD TMP_M8(A, sub8)               /* CMPA  */
  case 0xB2:       EXTENDD REG_M8(A, sbc8)               /* SBCA  */
  case 0xB3:       EXTENDD REG_M16(D, sub16)             /* SUBD  */
  case 0xB4:       EXTENDD REG_M8(A, and8)               /* ANDA  */
  case 0xB5:       EXTENDD TMP_M8(A, and8)               /* BITA  */
  case 0xB6:       EXTENDD REG_M8(A, ld8)                /* LDA   */
  case 0xB7:       EXTENDD ST_8(A)                       /* STA   */
  case 0xB8:       EXTENDD REG_M8(A, eor8)               /* EORA  */
  case 0xB9:       EXTENDD REG_M8(A, adc8)               /* ADCA  */
  case 0xBA:       EXTENDD REG_M8(A, or8)                /* ORA   */
  case 0xBB:       EXTENDD REG_M8(A, add8)               /* ADDA  */
  case 0xBC:       EXTENDD TMP_M16(X, sub16)             /* CMPX  */
  case 0xBD:       EXTENDD JSR                           /* JSR   */
  case 0xBE:       EXTENDD REG_M16(X, ld16)              /* LDX   */
  case 0xBF:       EXTENDD ST_16(X)                      /* STX   */

  case 0xC0: IMM8          REG_I8(B, sub8)               /* SUBB  */
  case 0xC1: IMM8          TMP_I8(B, sub8)               /* CMPB  */
  case 0xC2: IMM8          REG_I8(B, sbc8)               /* SBCB  */
  case 0xC3: IMM16         REG_I16(D, add16)             /* ADDD  */
  case 0xC4: IMM8          REG_I8(B, and8)               /* ANDB  */
  case 0xC5: IMM8          TMP_I8(B, and8)               /* BITB  */
  case 0xC6: IMM8          REG_I8(B, ld8)                /* LDB   */
  case 0xC8: IMM8          REG_I8(B, eor8)               /* EORB  */
  case 0xC9: IMM8          REG_I8(B, adc8)               /* ADCB  */
  case 0xCA: IMM8          REG_I8(B, or8)                /* ORB   */
  case 0xCB: IMM8          REG_I8(B, add8)               /* ADDB  */
  case 0xCC: IMM16         REG_I16(D, ld16)              /* LDD   */
  case 0xCE: IMM16         REG_I16(U, ld16)              /* LDU   */

  case 0xD0:       DIRECT  REG_M8(B, sub8)               /* SUBB  */
  case 0xD1:       DIRECT  TMP_M8(B, sub8)               /* CMPB  */
  case 0xD2:       DIRECT  REG_M8(B, sbc8)               /* SBCB  */
  case 0xD3:       DIRECT  REG_M16(D, add16)             /* ADDD  */
  case 0xD4:       DIRECT  REG_M8(B, and8)               /* ANDB  */
  case 0xD5:       DIRECT  TMP_M8(B, and8)               /* BITB  */
  case 0xD6:       DIRECT  REG_M8(B, ld8)                /* LDB   */
  case 0xD7:       DIRECT  ST_8(B)                       /* STB   */
  case 0xD8:       DIRECT  REG_M8(B, eor8)               /* EORB  */
  case 0xD9:       DIRECT  REG_M8(B, adc8)               /* ADCB  */
  case 0xDA:       DIRECT  REG_M8(B, or8)                /* ORB   */
  case 0xDB:       DIRECT  REG_M8(B, add8)               /* ADDB  */
  case 0xDC:       DIRECT  REG_M16(D, ld16)              /* LDD   */
  case 0xDD:       DIRECT  ST_16(D)                      /* STD   */
  case 0xDE:       DIRECT  REG_M16(U, ld16)              /* LDU   */
  case 0xDF:       DIRECT  ST_16(U)                      /* STU   */

  case 0xE0:       INDEXED REG_M8(B, sub8)               /* SUBB  */
  case 0xE1:       INDEXED TMP_M8(B, sub8)               /* CMPB  */
  case 0xE2:       INDEXED REG_M8(B, sbc8)               /* SBCB  */
  case 0xE3:       INDEXED REG_M16(D, add16)             /* ADDD  */
  case 0xE4:       INDEXED REG_M8(B, and8)               /* ANDB  */
  case 0xE5:       INDEXED TMP_M8(B, and8)               /* BITB  */
  case 0xE6:       INDEXED REG_M8(B, ld8)                /* LDB   */
  case 0xE7:       INDEXED ST_8(B)                       /* STB   */
  case 0xE8:       INDEXED REG_M8(B, eor8)               /* EORB  */
  case 0xE9:       INDEXED REG_M8(B, adc8)               /* ADCB  */
  case 0xEA:       INDEXED REG_M8(B, or8)                /* ORB   */
  case 0xEB:       INDEXED REG_M8(B, add8)               /* ADDB  */
  case 0xEC:       INDEXED REG_M16(D, ld16)              /* LDD   */
  case 0xED:       INDEXED ST_16(D)                      /* STD   */
  case 0xEE:       INDEXED REG_M16(U, ld16)              /* LDU   */
  case 0xEF:       INDEXED ST_16(U)                      /* STU   */

  case 0xF0:       EXTENDD REG_M8(B, sub8)               /* SUBB  */
  case 0xF1:       EXTENDD TMP_M8(B, sub8)               /* CMPB  */
  case 0xF2:       EXTENDD REG_M8(B, sbc8)               /* SBCB  */
  case 0xF3:       EXTENDD REG_M16(D, add16)             /* ADDD  */
  case 0xF4:       EXTENDD REG_M8(B, and8)               /* ANDB  */
  case 0xF5:       EXTENDD TMP_M8(B, and8)               /* BITB  */
  case 0xF6:       EXTENDD REG_M8(B, ld8)                /* LDB   */
  case 0xF7:       EXTENDD ST_8(B)                       /* STB   */
  case 0xF8:       EXTENDD REG_M8(B, eor8)               /* EORB  */
  case 0xF9:       EXTENDD REG_M8(B, adc8)               /* ADCB  */
  case 0xFA:       EXTENDD REG_M8(B, or8)                /* ORB   */
  case 0xFB:       EXTENDD REG_M8(B, add8)               /* ADDB  */
  case 0xFC:       EXTENDD REG_M16(D, ld16)              /* LDD   */
  case 0xFD:       EXTENDD ST_16(D)                      /* STD   */
  case 0xFE:       EXTENDD REG_M16(U, ld16)              /* LDU   */
  case 0xFF:       EXTENDD ST_16(U)                      /* STU   */

  default:
    fprintf(stderr, "ILLEGAL OPCODE: %02X\n",
      (int)read_byte(mpu, mpu->PC - 1));
    exit(1);
  }
}

static void prefix_10(mpu6809 *mpu) {
  uint16_t imm16;
  uint16_t ea;
  switch (POP_b(PC)) {
  case 0x21: IMM16         LBR(0)                        /* LBRN  */
  case 0x22: IMM16         LBR(COND_HI)                  /* LBHI  */
  case 0x23: IMM16         LBR(COND_LS)                  /* LBLS  */
  case 0x24: IMM16         LBR(COND_CC)                  /* LBCC  */
  case 0x25: IMM16         LBR(COND_CS)                  /* LBCS  */
  case 0x26: IMM16         LBR(COND_NE)                  /* LBNE  */
  case 0x27: IMM16         LBR(COND_EQ)                  /* LBEQ  */
  case 0x28: IMM16         LBR(COND_VC)                  /* LBVC  */
  case 0x29: IMM16         LBR(COND_VS)                  /* LBVS  */
  case 0x2A: IMM16         LBR(COND_PL)                  /* LBPL  */
  case 0x2B: IMM16         LBR(COND_MI)                  /* LBMI  */
  case 0x2C: IMM16         LBR(COND_GE)                  /* LBGE  */
  case 0x2D: IMM16         LBR(COND_LT)                  /* LBLT  */
  case 0x2E: IMM16         LBR(COND_GT)                  /* LBGT  */
  case 0x2F: IMM16         LBR(COND_LE)                  /* LBLE  */

  case 0x3F: mpu->handle_swi2(mpu->context, mpu); return;/* SWI2  */

  case 0x83: IMM16         TMP_I16(D, sub16)             /* CMPD  */
  case 0x8C: IMM16         TMP_I16(Y, sub16)             /* CMPY  */
  case 0x8E: IMM16         REG_I16(Y, ld16)              /* LDY   */

  case 0x93:       DIRECT  TMP_M16(D, sub16)             /* CMPD  */
  case 0x9C:       DIRECT  TMP_M16(Y, sub16)             /* CMPY  */
  case 0x9E:       DIRECT  REG_M16(Y, ld16)              /* LDY   */
  case 0x9F:       DIRECT  ST_16(Y)                      /* STY   */

  case 0xA3:       INDEXED TMP_M16(D, sub16)             /* CMPD  */
  case 0xAC:       INDEXED TMP_M16(Y, sub16)             /* CMPY  */
  case 0xAE:       INDEXED REG_M16(Y, ld16)              /* LDY   */
  case 0xAF:       INDEXED ST_16(Y)                      /* STY   */

  case 0xB3:       EXTENDD TMP_M16(D, sub16)             /* CMPD  */
  case 0xBC:       EXTENDD TMP_M16(Y, sub16)             /* CMPY  */
  case 0xBE:       EXTENDD REG_M16(Y, ld16)              /* LDY   */
  case 0xBF:       EXTENDD ST_16(Y)                      /* STY   */

  case 0xCE: IMM16         REG_I16(S, ld16)              /* LDS   */

  case 0xDE:       DIRECT  REG_M16(S, ld16)              /* LDS   */
  case 0xDF:       DIRECT  ST_16(S)                      /* STS   */

  case 0xEE:       INDEXED REG_M16(S, ld16)              /* LDS   */
  case 0xEF:       INDEXED ST_16(S)                      /* STS   */

  case 0xFE:       EXTENDD REG_M16(S, ld16)              /* LDS   */
  case 0xFF:       EXTENDD ST_16(S)                      /* STS   */

  default:
    fprintf(stderr, "ILLEGAL OPCODE: 10 %02X\n",
      (int)read_byte(mpu, mpu->PC - 1));
    exit(1);
  }
}

static void prefix_11(mpu6809 *mpu) {
  uint16_t imm16;
  uint16_t ea;
  switch (POP_b(PC)) {
  case 0x3F: mpu->handle_swi3(mpu->context, mpu); return;/* SWI3  */

  case 0x83: IMM16         TMP_I16(U, sub16)             /* CMPU  */
  case 0x8C: IMM16         TMP_I16(S, sub16)             /* CMPS  */

  case 0x93:       DIRECT  TMP_M16(U, sub16)             /* CMPU  */
  case 0x9C:       DIRECT  TMP_M16(S, sub16)             /* CMPS  */

  case 0xA3:       INDEXED TMP_M16(U, sub16)             /* CMPU  */
  case 0xAC:       INDEXED TMP_M16(S, sub16)             /* CMPS  */

  case 0xB3:       EXTENDD TMP_M16(U, sub16)             /* CMPU  */
  case 0xBC:       EXTENDD TMP_M16(S, sub16)             /* CMPS  */
  default:
    fprintf(stderr, "ILLEGAL OPCODE: 11 %02X\n",
      (int)read_byte(mpu, mpu->PC - 1));
    exit(1);
  }
}
