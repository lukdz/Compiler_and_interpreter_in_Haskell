#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpu6809.h"
#include "ROM.h"

#define DBG_BUFSIZE 64

#define ROM_ADDRESS      0xF800
#define PROGRAM_SIZE_HI  0xF7FC
#define PROGRAM_SIZE_LO  0xF7FD
#define CONSOLE_IN_ADDR  0xF7FE
#define CONSOLE_OUT_ADDR 0xF7FE
#define STATUS_BYTE_ADDR 0xF7FF

static int debugFlag = 0;
static const char *code_path;

static uint8_t memory[0x10000];
static uint16_t code_size;

static uint8_t read_memory(void *context, uint16_t addr) {
  if (addr >= ROM_ADDRESS && addr < ROM_ADDRESS + ROM_bin_len) {
    return ROM_bin[addr - ROM_ADDRESS];
  } else if (addr == PROGRAM_SIZE_HI) {
    return code_size >> 8;
  } else if (addr == PROGRAM_SIZE_LO) {
    return code_size;
  } else if (addr == CONSOLE_IN_ADDR) {
    return fgetc(stdin);
  } else {
    return memory[addr];
  }
}

static void write_memory(void *context, uint16_t addr, uint8_t value) {
  if (addr >= ROM_ADDRESS && addr < ROM_ADDRESS + ROM_bin_len) {
  } else if (addr == CONSOLE_OUT_ADDR) {
    fputc(value, stdout);
  } else if (addr == STATUS_BYTE_ADDR) {
    exit(0);
  } else {
    memory[addr] = value;
  }
}

static int parse_args(int argc, char **argv) {
  if (argc > 0 && strcmp(argv[0], "-d") == 0) {
    debugFlag = 1;
    argc--;
    argv++;
  }
  if (argc == 1 && argv[0][0] != '-') {
    code_path = argv[0];
    return 0;
  } else {
    return 1;
  }
}

static void load_code(const char *fname) {
  int s;
  FILE *file = fopen(fname, "rb");
  if (!file) {
    perror("cannot open file");
    exit(1);
  }
  code_size = 0;
  do {
    s = fread(memory + code_size, 1, 0x10000 - code_size, file);
    code_size += s;
  } while (s > 0);
  fclose(file);
}

static void show_data(uint16_t addr) {
  uint16_t i;
  printf("%04X  ", addr);
  for (i = 0; i < 8; ++i) {
    printf("%02X%02X ",
      read_memory(0, addr + 2*i),
      read_memory(0, addr + 2*i + 1));
    if (i == 3) {
      printf(" ");
    }
  }
  printf("  ");
  for (i = 0; i < 16; ++i) {
    char c = read_memory(0, (uint16_t)(addr + i));
    printf("%c", (c >= 32 && c <= 127 ? c : '.'));
    if (i == 7) {
      printf(" ");
    }
  }
  printf("\n");
}

static int debugger(mpu6809 *mpu) {
  static char buffer[DBG_BUFSIZE];
  char c;
  unsigned n;
  for (;;) {
    printf("X:  "); show_data(mpu->X);
    printf("Y:  "); show_data(mpu->Y);
    printf("S:  "); show_data(mpu->S);
    printf("U:  "); show_data(mpu->U);
    printf("PC: "); show_data(mpu->PC);
    printf("D:  "); show_data(mpu->D);
    printf("DP: %02X\n", mpu->DP);
    printf("CC: %02X\n", mpu->CC);
    printf("> ");
    if (!fgets(buffer, DBG_BUFSIZE, stdin)) {
      printf("\n");
      return 0;
    }
    if (!sscanf(buffer, " %c", &c)) {
      continue;
    }
    switch (c) {
    case 'q': return 0;
    case 's':
      if (sscanf(buffer, " s %u", &n) < 1) {
        n = 1;
      }
      for (; n; --n) {
        mpu6809_step(mpu);
      }
      break;
    case 'n': {
        uint16_t pc = mpu->PC + 1;
        while (mpu->PC < pc || mpu->PC > pc + 4) {
          mpu6809_step(mpu);
        }
      }
      break;
    case 'g':
      if (sscanf(buffer, " g %x", &n) >= 1) {
        while (mpu->PC != n) {
          mpu6809_step(mpu);
        }
      }
      break;
    case 'd':
      if (sscanf(buffer, " d %x", &n) >= 1) {
        show_data(n);
      }
      break;
    }
  }
}

int main(int argc, char **argv) {
  if (parse_args(argc-1, argv+1)) {
    fprintf(stderr, "Usage: emu6809 [-d] FILE\n");
    exit(1);
  }

  load_code(code_path);

  mpu6809 mpu;
  mpu6809_init(&mpu);
  mpu.read_mem = read_memory;
  mpu.write_mem = write_memory;
  
  mpu6809_reset(&mpu);
  if (debugFlag) {
    return debugger(&mpu);
  } else {
    for (;;) {
      mpu6809_step(&mpu);
    }
  }
}
