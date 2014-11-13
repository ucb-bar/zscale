// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._
import rocket.Util._
import rocket.ALU._
import rocket.Instructions._

class CtrlDpathIO extends Bundle with ZScaleParameters
{
  val j = Bool(OUTPUT)
  val br = Bool(OUTPUT)
  val sel_alu1 = UInt(OUTPUT, 2)
  val sel_alu2 = UInt(OUTPUT, 3)
  val sel_imm = UInt(OUTPUT, 3)
  val fn_alu = UInt(OUTPUT, SZ_ALU_FN)
  val wen = Bool(OUTPUT)
  val csr_en = Bool(OUTPUT)
  val csr_cmd = UInt(OUTPUT, CSR.SZ)
  val mem_valid = Bool(OUTPUT)
  val mem_rw = Bool(OUTPUT)
  val mem_type = UInt(OUTPUT, MT_SZ)
  val mul_valid = Bool(OUTPUT)
  val xcpt = Bool(OUTPUT)
  val cause = UInt(OUTPUT, xprLen)
  val sret = Bool(OUTPUT)

  val stallf = Bool(OUTPUT)
  val killf = Bool(OUTPUT)
  val stalldx = Bool(OUTPUT)
  val killdx = Bool(OUTPUT)

  val status = new Status().asInput
  val inst = Bits(INPUT, coreInstBits)
  val ma_pc = Bool(INPUT)
  val fa_pc = Bool(INPUT)
  val ma_addr = Bool(INPUT)
  val fa_addr = Bool(INPUT)
  val br_taken = Bool(INPUT)
  val mul_ready = Bool(INPUT)
  val clear_sb = Bool(INPUT)

  // for logging purposes
  val invalidate = Bool(OUTPUT)
  val sb_stall = Bool(OUTPUT)
  val scr_stall = Bool(OUTPUT)
  val imem_stall = Bool(OUTPUT)
  val dmem_stall = Bool(OUTPUT)
  val mul_stall = Bool(OUTPUT)
}

class Control extends Module with ZScaleParameters
{
  val io = new Bundle {
    val dpath = new CtrlDpathIO
    val imem = new InstMemIO
    val dmem = new ScratchPadIO
    val dmem_fast_arb = Bool(OUTPUT)
    val host = new HTIFIO
    val scr = new SCRIO
    val scr_ready = Bool(INPUT)
  }

  io.imem.req.valid := Bool(true)

  val id_valid = Reg(init = Bool(false))

  when (!io.dpath.stalldx) {
    id_valid := !io.dpath.killf
  }

  val cs = DecodeLogic(io.dpath.inst,
                //  val j br si f.i sc sb sr csr    s_alu1   s_alu2   imm     fn       wen sb mem rw mtype  mul
                //   |  |  |  |  |  |  |  |  |      |        |        |       |          |  |  |  |  |      |
                List(N, X, X, X, X, X, X, X, CSR.X, A1_X,    A2_X,    IMM_X,  FN_X,      X, X, X, X, MT_X,  X), Array(
      LUI->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_ZERO, A2_IMM,  IMM_U,  FN_ADD,    Y, N, N, X, MT_X,  N),
      AUIPC->   List(Y, N, N, N, N, N, N, N, CSR.N, A1_PC,   A2_IMM,  IMM_U,  FN_ADD,    Y, N, N, X, MT_X,  N),

      JAL->     List(Y, Y, N, N, N, N, N, N, CSR.N, A1_PC,   A2_FOUR, IMM_UJ, FN_ADD,    Y, N, N, X, MT_X,  N),
      JALR->    List(Y, Y, N, N, N, N, N, N, CSR.N, A1_PC,   A2_FOUR, IMM_I,  FN_ADD,    Y, N, N, X, MT_X,  N),

      BEQ->     List(Y, N, Y, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SEQ,    N, N, N, X, MT_X,  N),
      BNE->     List(Y, N, Y, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SNE,    N, N, N, X, MT_X,  N),
      BLT->     List(Y, N, Y, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SLT,    N, N, N, X, MT_X,  N),
      BLTU->    List(Y, N, Y, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SLTU,   N, N, N, X, MT_X,  N),
      BGE->     List(Y, N, Y, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SGE,    N, N, N, X, MT_X,  N),
      BGEU->    List(Y, N, Y, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SGEU,   N, N, N, X, MT_X,  N),

      LB->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_B,  N),
      LBU->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_BU, N),
      LH->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_H,  N),
      LHU->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_HU, N),
      LW->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, Y, N, MT_W,  N),
      SB->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, N, Y, Y, MT_B,  N),
      SH->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, N, Y, Y, MT_H,  N),
      SW->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, N, Y, Y, MT_W,  N),

      ADDI->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    Y, N, N, X, MT_X,  N),
      SLTI->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SLT,    Y, N, N, X, MT_X,  N),
      SLTIU->   List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SLTU,   Y, N, N, X, MT_X,  N),
      XORI->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_XOR,    Y, N, N, X, MT_X,  N),
      ORI->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_OR,     Y, N, N, X, MT_X,  N),
      ANDI->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_AND,    Y, N, N, X, MT_X,  N),
      SLLI->    List(Y, N, N, Y, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SL,     Y, N, N, X, MT_X,  N),
      SRLI->    List(Y, N, N, Y, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SR,     Y, N, N, X, MT_X,  N),
      SRAI->    List(Y, N, N, Y, N, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SRA,    Y, N, N, X, MT_X,  N),

      ADD->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_ADD,    Y, N, N, X, MT_X,  N),
      SUB->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SUB,    Y, N, N, X, MT_X,  N),
      SLL->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SL,     Y, N, N, X, MT_X,  N),
      SLT->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SLT,    Y, N, N, X, MT_X,  N),
      SLTU->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SLTU,   Y, N, N, X, MT_X,  N),
      XOR->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_XOR,    Y, N, N, X, MT_X,  N),
      SRL->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SR,     Y, N, N, X, MT_X,  N),
      SRA->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SRA,    Y, N, N, X, MT_X,  N),
      OR->      List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_OR,     Y, N, N, X, MT_X,  N),
      AND->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_AND,    Y, N, N, X, MT_X,  N),

      FENCE->   List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N), // nop
      FENCE_I-> List(Y, N, N, N, Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N),

      SCALL->   List(Y, N, N, N, N, Y, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N),
      SBREAK->  List(Y, N, N, N, N, N, Y, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N),
      SRET->    List(Y, N, N, N, N, N, N, Y, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, X, MT_X,  N),

      CSRRW->   List(Y, N, N, N, N, N, N, N, CSR.W, A1_RS1,  A2_ZERO, IMM_X,  FN_ADD,    Y, N, N, X, MT_X,  N),
      CSRRS->   List(Y, N, N, N, N, N, N, N, CSR.S, A1_RS1,  A2_ZERO, IMM_X,  FN_ADD,    Y, N, N, X, MT_X,  N),
      CSRRC->   List(Y, N, N, N, N, N, N, N, CSR.C, A1_RS1,  A2_ZERO, IMM_X,  FN_ADD,    Y, N, N, X, MT_X,  N),
      CSRRWI->  List(Y, N, N, N, N, N, N, N, CSR.W, A1_ZERO, A2_IMM,  IMM_Z,  FN_ADD,    Y, N, N, X, MT_X,  N),
      CSRRSI->  List(Y, N, N, N, N, N, N, N, CSR.S, A1_ZERO, A2_IMM,  IMM_Z,  FN_ADD,    Y, N, N, X, MT_X,  N),
      CSRRCI->  List(Y, N, N, N, N, N, N, N, CSR.C, A1_ZERO, A2_IMM,  IMM_Z,  FN_ADD,    Y, N, N, X, MT_X,  N),

      MUL->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MUL,    N, Y, N, X, MT_X,  Y),
      MULH->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MULH,   N, Y, N, X, MT_X,  Y),
      MULHU->   List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MULHU,  N, Y, N, X, MT_X,  Y),
      MULHSU->  List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MULHSU, N, Y, N, X, MT_X,  Y),
      DIV->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_DIV,    N, Y, N, X, MT_X,  Y),
      DIVU->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_DIVU,   N, Y, N, X, MT_X,  Y),
      REM->     List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_REM,    N, Y, N, X, MT_X,  Y),
      REMU->    List(Y, N, N, N, N, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_REMU,   N, Y, N, X, MT_X,  Y)
    ))

  val (_id_inst_valid: Bool) :: (id_j: Bool) :: (id_br: Bool) :: (id_shift_imm: Bool) :: cs1 = cs
  val (id_fence_i: Bool) :: (id_scall: Bool) :: (id_sbreak: Bool) :: (id_sret: Bool) :: id_csr :: cs2 = cs1
  val id_sel_alu1 :: id_sel_alu2 :: id_sel_imm :: id_fn_alu :: (id_wen: Bool) :: cs3 = cs2
  val (id_set_sb: Bool) :: (id_mem_valid: Bool) :: (id_mem_rw: Bool) :: id_mem_type :: (id_mul_valid: Bool) :: Nil = cs3

  val sr = io.dpath.status
  val sb_stall = Reg(init = Bool(false))

  // we need this because we're using RV64I's shift instruction format
  val id_shift_valid = !id_shift_imm || io.dpath.inst(25) === Bits(0) // checking whether shamt's bit 6 is a zero

  val id_csr_addr = io.dpath.inst(31, 20)
  val id_raddr1 = io.dpath.inst(19, 15)
  val legal_csrs = collection.mutable.LinkedHashSet(csrList:_*)
  val is_legal_csr = Vec.tabulate(1 << id_csr_addr.getWidth)(i => Bool(legal_csrs contains i))
  val id_csr_en = id_csr != CSR.N
  val id_csr_wen = id_raddr1 != UInt(0) || !Vec(CSR.S, CSR.C).contains(id_csr)
  val id_csr_valid = !id_csr_en || is_legal_csr(id_csr_addr)
  val id_csr_privileged = id_csr_en &&
    (id_csr_addr(11, 10) === UInt(3) && id_csr_wen ||
     id_csr_addr(11, 10) === UInt(1) && !sr.s ||
     id_csr_addr(9, 8) === UInt(1) && !sr.s)

  val id_inst_valid = _id_inst_valid && id_shift_valid && id_csr_valid
  val id_ok = !sb_stall && id_valid && id_inst_valid

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  var id_interrupts = (0 until sr.ip.getWidth).map(i => (sr.im(i) && sr.ip(i), UInt(BigInt(1) << (xprLen-1) | i)))
  val (id_interrupt_unmasked, id_interrupt_cause) = checkExceptions(id_interrupts)
  val id_interrupt = sr.ei && id_interrupt_unmasked

  val (id_xcpt, id_cause) = checkExceptions(List(
    (id_interrupt, id_interrupt_cause),
    (io.dpath.ma_pc, UInt(Causes.misaligned_fetch)),
    (io.dpath.fa_pc, UInt(Causes.fault_fetch)),
    (!sb_stall && id_valid && !id_inst_valid, UInt(Causes.illegal_instruction)),
    (id_ok && id_csr_privileged, UInt(Causes.privileged_instruction)),
    (id_ok && id_sret && !sr.s, UInt(Causes.privileged_instruction)),
    (id_ok && id_scall, UInt(Causes.syscall)),
    (id_ok && id_sbreak, UInt(Causes.breakpoint)),
    (id_ok && id_mem_valid && !id_mem_rw && io.dpath.ma_addr, UInt(Causes.misaligned_load)),
    (id_ok && id_mem_valid &&  id_mem_rw && io.dpath.ma_addr, UInt(Causes.misaligned_store)),
    (id_ok && id_mem_valid && !id_mem_rw && io.dpath.fa_addr, UInt(Causes.fault_load)),
    (id_ok && id_mem_valid &&  id_mem_rw && io.dpath.fa_addr, UInt(Causes.fault_store)) ))

  val id_retire = id_ok && !id_xcpt

  io.dpath.j := id_retire && id_j
  io.dpath.br := id_retire && id_br
  io.dpath.sel_alu1 := id_sel_alu1
  io.dpath.sel_alu2 := id_sel_alu2
  io.dpath.sel_imm := id_sel_imm
  io.dpath.fn_alu := id_fn_alu
  io.dpath.wen := id_retire && id_wen
  io.dpath.csr_en := id_retire && id_csr_en
  io.dpath.csr_cmd := Mux(id_retire, id_csr, CSR.N)
  io.dpath.mem_valid := id_retire && id_mem_valid
  io.dpath.mem_rw := id_mem_rw
  io.dpath.mem_type := id_mem_type
  io.dpath.mul_valid := id_retire && id_mul_valid
  io.dpath.xcpt := id_xcpt
  io.dpath.cause := id_cause
  io.dpath.sret := id_retire && id_sret

  io.imem.invalidate.valid := id_retire && id_fence_i
  io.dmem.req.valid := io.dpath.mem_valid
  io.dmem_fast_arb := id_ok && id_mem_valid

  when (!io.dpath.killdx && id_set_sb) {
    sb_stall := Bool(true)
  }
  when (io.dpath.clear_sb) {
    sb_stall := Bool(false)
  }

  val scr_stall = io.dpath.csr_en && (!io.scr_ready || !io.host.ipi_req.ready)
  val imem_stall = io.imem.invalidate.valid && !io.imem.invalidate.ready
  val dmem_stall = io.dmem.req.valid && !io.dmem.req.ready
  val mul_stall = io.dpath.mul_valid && !io.dpath.mul_ready

  val br_taken = io.dpath.br && io.dpath.br_taken
  val redirect = io.dpath.j || br_taken || id_xcpt || io.dpath.sret
  io.dpath.stallf := !redirect && (!io.imem.resp.valid || io.imem.invalidate.valid || io.dpath.stalldx)
  io.dpath.killf := !io.imem.resp.valid || io.imem.invalidate.valid || redirect
  io.dpath.stalldx := sb_stall || scr_stall || imem_stall || dmem_stall || mul_stall
  io.dpath.killdx := !id_retire || io.dpath.stalldx

  // for logging purposes
  io.dpath.invalidate := io.imem.invalidate.valid
  io.dpath.sb_stall := sb_stall
  io.dpath.scr_stall := scr_stall
  io.dpath.imem_stall := imem_stall
  io.dpath.dmem_stall := dmem_stall
  io.dpath.mul_stall := mul_stall
}
