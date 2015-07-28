#include "encoding.h"

#define SYS_exit 93

void tohost_exit(long code)
{
  write_csr(mtohost, (code << 1) | 1);
  while (1);
}

long handle_trap(long cause, long epc, long long regs[32])
{
  if (cause != CAUSE_USER_ECALL)
    tohost_exit(1337);
  else if (regs[17] == SYS_exit)
    tohost_exit(regs[10]);

  return epc+4;
}

static long syscall(long num, long arg0, long arg1, long arg2)
{
  register long a7 asm("a7") = num;
  register long a0 asm("a0") = arg0;
  register long a1 asm("a1") = arg1;
  register long a2 asm("a2") = arg2;
  asm volatile ("scall" : "+r"(a0) : "r"(a1), "r"(a2), "r"(a7));
  return a0;
}

void exit(int code)
{
  syscall(SYS_exit, code, 0, 0);
  while (1);
}

int __attribute__((weak)) main()
{
  return -1;
}

void _init()
{
  exit(main());
}
