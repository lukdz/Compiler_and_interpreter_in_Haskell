#ifndef MPU6809_H_INCLUDED
#define MPU6809_H_INCLUDED

#include <stdint.h>

struct _mpu6809;

typedef uint8_t (*read_mem_t)(void *context, uint16_t address);
typedef void (*write_mem_t)(void *context, uint16_t address, uint8_t value);
typedef void (*handle_swi_t)(void *context, struct _mpu6809 *mpu);

typedef struct _mpu6809 {
  uint16_t X;
  uint16_t Y;
  uint16_t U;
  uint16_t S;
  uint16_t PC;

  union {
    uint16_t D;
    struct {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ \
  || __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
      uint8_t B;
      uint8_t A;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
      uint8_t A;
      uint8_t B;
#else
#error "Cannot detect endianness"
#endif
    };
  };

  uint8_t DP;
  uint8_t CC;

  void *context;
  read_mem_t   read_mem;
  write_mem_t  write_mem;
  handle_swi_t handle_swi;
  handle_swi_t handle_swi2;
  handle_swi_t handle_swi3;
} mpu6809;

void mpu6809_init(mpu6809 *mpu);
void mpu6809_reset(mpu6809 *mpu);
void mpu6809_step(mpu6809 *mpu);

void mpu6809_default_handle_swi(void *context, mpu6809 *mpu);
void mpu6809_default_handle_swi2(void *context, mpu6809 *mpu);
void mpu6809_default_handle_swi3(void *context, mpu6809 *mpu);

#endif
