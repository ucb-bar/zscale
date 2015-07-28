# See LICENSE for license details.

#include "encoding.h"

  .text
  .align 6
user_trap_entry:
  j trap_entry

  .align 6
supervisor_trap_entry:
  j supervisor_trap_entry

  .align 6
hypervisor_trap_entry:
  j hypervisor_trap_entry

  .align 6
machine_trap_entry:
  j trap_entry

  .align 6
  .globl _start
_start:
  li  x1, 0
  li  x2, 0
  li  x3, 0
  li  x4, 0
  li  x5, 0
  li  x6, 0
  li  x7, 0
  li  x8, 0
  li  x9, 0
  li  x10,0
  li  x11,0
  li  x12,0
  li  x13,0
  li  x14,0
  li  x15,0
  li  x16,0
  li  x17,0
  li  x18,0
  li  x19,0
  li  x20,0
  li  x21,0
  li  x22,0
  li  x23,0
  li  x24,0
  li  x25,0
  li  x26,0
  li  x27,0
  li  x28,0
  li  x29,0
  li  x30,0
  li  x31,0

  li t0, MSTATUS_PRV1; csrc mstatus, t0    # run tests in user mode
  li t0, MSTATUS_IE1;  csrs mstatus, t0    # enable interrupts in user mode

  # initialize global pointer
  la gp, _gp

  la  tp, _end + 63
  and tp, tp, -64

  # get core id
  csrr a0, mhartid
  # for now, assume only 1 core
  li a1, 1
1:bgeu a0, a1, 1b

  # give each core 2KB of stack + TLS
#define STKSHIFT 11
  sll a2, a0, STKSHIFT
  add tp, tp, a2
  add sp, a0, 1
  sll sp, sp, STKSHIFT
  add sp, sp, tp

  la t0, _init
  csrw mepc, t0
  eret

trap_entry:
  addi sp, sp, -272

  sw x1, 8(sp)
  sw x2, 16(sp)
  sw x3, 24(sp)
  sw x4, 32(sp)
  sw x5, 40(sp)
  sw x6, 48(sp)
  sw x7, 56(sp)
  sw x8, 64(sp)
  sw x9, 72(sp)
  sw x10, 80(sp)
  sw x11, 88(sp)
  sw x12, 96(sp)
  sw x13, 104(sp)
  sw x14, 112(sp)
  sw x15, 120(sp)
  sw x16, 128(sp)
  sw x17, 136(sp)
  sw x18, 144(sp)
  sw x19, 152(sp)
  sw x20, 160(sp)
  sw x21, 168(sp)
  sw x22, 176(sp)
  sw x23, 184(sp)
  sw x24, 192(sp)
  sw x25, 200(sp)
  sw x26, 208(sp)
  sw x27, 216(sp)
  sw x28, 224(sp)
  sw x29, 232(sp)
  sw x30, 240(sp)
  sw x31, 248(sp)

  csrr a0, mcause
  csrr a1, mepc
  mv a2, sp
  jal handle_trap
  csrw mepc, a0

  lw x1, 8(sp)
  lw x2, 16(sp)
  lw x3, 24(sp)
  lw x4, 32(sp)
  lw x5, 40(sp)
  lw x6, 48(sp)
  lw x7, 56(sp)
  lw x8, 64(sp)
  lw x9, 72(sp)
  lw x10, 80(sp)
  lw x11, 88(sp)
  lw x12, 96(sp)
  lw x13, 104(sp)
  lw x14, 112(sp)
  lw x15, 120(sp)
  lw x16, 128(sp)
  lw x17, 136(sp)
  lw x18, 144(sp)
  lw x19, 152(sp)
  lw x20, 160(sp)
  lw x21, 168(sp)
  lw x22, 176(sp)
  lw x23, 184(sp)
  lw x24, 192(sp)
  lw x25, 200(sp)
  lw x26, 208(sp)
  lw x27, 216(sp)
  lw x28, 224(sp)
  lw x29, 232(sp)
  lw x30, 240(sp)
  lw x31, 248(sp)

  addi sp, sp, 272
  eret

.section ".tdata.begin"
.globl _tdata_begin
_tdata_begin:

.section ".tdata.end"
.globl _tdata_end
_tdata_end:

.section ".tbss.end"
.globl _tbss_end
_tbss_end:
