// See LICENSE for license details.

package zscale

import Chisel._
import cde.Parameters
import junctions._
import uncore._
import rocket._
import rocket.Util._
import rocket.ALU._
import rocket.Instructions._

class CtrlDpathIO(implicit p: Parameters) extends ZscaleBundle {
  val stallf = Bool(OUTPUT)
  val killf = Bool(OUTPUT)
  val stalldx = Bool(OUTPUT)
  val killdx = Bool(OUTPUT)
  val stallw = Bool(OUTPUT)

  val id = new Bundle {
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
    val cause = UInt(OUTPUT, xLen)
  }

  val ll = new Bundle {
    val valid = Bool(OUTPUT)
    val waddr = UInt(OUTPUT, 5)
    val fn = Bool(OUTPUT)
    val mem_rw = Bool(OUTPUT)
    val mem_type = UInt(OUTPUT, MT_SZ)
  }

  val inst = Bits(INPUT, coreInstBits)
  val ma_pc = Bool(INPUT)
  val ma_addr = Bool(INPUT)
  val br_taken = Bool(INPUT)
  val mul_ready = Bool(INPUT)
  val clear_sb = Bool(INPUT)
  val csr_xcpt = Bool(INPUT)
  val csr_eret = Bool(INPUT)
  val csr_interrupt = Bool(INPUT)
  val csr_interrupt_cause = UInt(INPUT, xLen)

  val logging = new Bundle {
    val invalidate = Bool(OUTPUT)
    val sb_stall = Bool(OUTPUT)
    val dmem_stall = Bool(OUTPUT)
    val mul_stall = Bool(OUTPUT)
  }
}

class Control(implicit p: Parameters) extends ZscaleModule()(p) {
  val io = new Bundle {
    val dpath = new CtrlDpathIO
    val imem = new HastiMasterIO
    val dmem = new HastiMasterIO
    val host = new HtifIO
  }

  io.imem.hwrite := Bool(false)
  io.imem.hsize := UInt("b010")
  io.imem.hburst := HBURST_SINGLE
  io.imem.hprot := UInt("b0011")
  io.imem.htrans := HTRANS_NONSEQ
  io.imem.hmastlock := Bool(false)

  val if_kill = Reg(init = Bool(true))
  val id_valid = Reg(init = Bool(false))

  when (!io.dpath.stalldx) {
    id_valid := !io.dpath.killf
  }

                  //  val j br f.i csr    s_alu1   s_alu2   imm     fn       wen r1 r2 sb mem rw mtype  mul
                  //   |  |  |  |  |      |        |        |       |          |  |  |  |  |  |  |      |
  val decode_default: List[BitPat] =
                  List(N, X, X, X, CSR.X, A1_X,    A2_X,    IMM_X,  FN_X,      X, X, X, X, X, X, MT_X,  X)
  val mulDivDecodeTable: Array[(BitPat, List[BitPat])] = Array(
      MUL->       List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MUL,    N, Y, Y, Y, N, X, MT_X,  Y),
      MULH->      List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MULH,   N, Y, Y, Y, N, X, MT_X,  Y),
      MULHU->     List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MULHU,  N, Y, Y, Y, N, X, MT_X,  Y),
      MULHSU->    List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_MULHSU, N, Y, Y, Y, N, X, MT_X,  Y),
      DIV->       List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_DIV,    N, Y, Y, Y, N, X, MT_X,  Y),
      DIVU->      List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_DIVU,   N, Y, Y, Y, N, X, MT_X,  Y),
      REM->       List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_REM,    N, Y, Y, Y, N, X, MT_X,  Y),
      REMU->      List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_REMU,   N, Y, Y, Y, N, X, MT_X,  Y))
  val intDecodeTable: Array[(BitPat, List[BitPat])] = Array(
      LUI->       List(Y, N, N, N, CSR.N, A1_ZERO, A2_IMM,  IMM_U,  FN_ADD,    Y, N, N, N, N, X, MT_X,  N),
      AUIPC->     List(Y, N, N, N, CSR.N, A1_PC,   A2_IMM,  IMM_U,  FN_ADD,    Y, N, N, N, N, X, MT_X,  N),

      JAL->       List(Y, Y, N, N, CSR.N, A1_PC,   A2_FOUR, IMM_UJ, FN_ADD,    Y, N, N, N, N, X, MT_X,  N),
      JALR->      List(Y, Y, N, N, CSR.N, A1_PC,   A2_FOUR, IMM_I,  FN_ADD,    Y, Y, N, N, N, X, MT_X,  N),

      BEQ->       List(Y, N, Y, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SEQ,    N, Y, Y, N, N, X, MT_X,  N),
      BNE->       List(Y, N, Y, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SNE,    N, Y, Y, N, N, X, MT_X,  N),
      BLT->       List(Y, N, Y, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SLT,    N, Y, Y, N, N, X, MT_X,  N),
      BLTU->      List(Y, N, Y, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SLTU,   N, Y, Y, N, N, X, MT_X,  N),
      BGE->       List(Y, N, Y, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SGE,    N, Y, Y, N, N, X, MT_X,  N),
      BGEU->      List(Y, N, Y, N, CSR.N, A1_RS1,  A2_RS2,  IMM_SB, FN_SGEU,   N, Y, Y, N, N, X, MT_X,  N),

      LB->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, N, Y, Y, N, MT_B,  N),
      LBU->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, N, Y, Y, N, MT_BU, N),
      LH->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, N, Y, Y, N, MT_H,  N),
      LHU->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, N, Y, Y, N, MT_HU, N),
      LW->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    N, Y, N, Y, Y, N, MT_W,  N),
      SB->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, Y, Y, Y, Y, Y, MT_B,  N),
      SH->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, Y, Y, Y, Y, Y, MT_H,  N),
      SW->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_S,  FN_ADD,    N, Y, Y, Y, Y, Y, MT_W,  N),

      ADDI->      List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_ADD,    Y, Y, N, N, N, X, MT_X,  N),
      SLTI->      List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SLT,    Y, Y, N, N, N, X, MT_X,  N),
      SLTIU->     List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SLTU,   Y, Y, N, N, N, X, MT_X,  N),
      XORI->      List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_XOR,    Y, Y, N, N, N, X, MT_X,  N),
      ORI->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_OR,     Y, Y, N, N, N, X, MT_X,  N),
      ANDI->      List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_AND,    Y, Y, N, N, N, X, MT_X,  N),
      SLLI_RV32-> List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SL,     Y, Y, N, N, N, X, MT_X,  N),
      SRLI_RV32-> List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SR,     Y, Y, N, N, N, X, MT_X,  N),
      SRAI_RV32-> List(Y, N, N, N, CSR.N, A1_RS1,  A2_IMM,  IMM_I,  FN_SRA,    Y, Y, N, N, N, X, MT_X,  N),

      ADD->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_ADD,    Y, Y, Y, N, N, X, MT_X,  N),
      SUB->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SUB,    Y, Y, Y, N, N, X, MT_X,  N),
      SLL->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SL,     Y, Y, Y, N, N, X, MT_X,  N),
      SLT->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SLT,    Y, Y, Y, N, N, X, MT_X,  N),
      SLTU->      List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SLTU,   Y, Y, Y, N, N, X, MT_X,  N),
      XOR->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_XOR,    Y, Y, Y, N, N, X, MT_X,  N),
      SRL->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SR,     Y, Y, Y, N, N, X, MT_X,  N),
      SRA->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_SRA,    Y, Y, Y, N, N, X, MT_X,  N),
      OR->        List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_OR,     Y, Y, Y, N, N, X, MT_X,  N),
      AND->       List(Y, N, N, N, CSR.N, A1_RS1,  A2_RS2,  IMM_X,  FN_AND,    Y, Y, Y, N, N, X, MT_X,  N),

      FENCE->     List(Y, N, N, N, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, N, N, X, MT_X,  N),
      FENCE_I->   List(Y, N, N, Y, CSR.N, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, N, N, X, MT_X,  N),

      SCALL->     List(Y, N, N, N, CSR.I, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, N, N, X, MT_X,  N),
      SBREAK->    List(Y, N, N, N, CSR.I, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, N, N, X, MT_X,  N),
      SRET->      List(Y, N, N, N, CSR.I, A1_X,    A2_X,    IMM_X,  FN_X,      N, N, N, N, N, X, MT_X,  N),

      CSRRW->     List(Y, N, N, N, CSR.W, A1_RS1,  A2_ZERO, IMM_X,  FN_ADD,    Y, Y, N, N, N, X, MT_X,  N),
      CSRRS->     List(Y, N, N, N, CSR.S, A1_RS1,  A2_ZERO, IMM_X,  FN_ADD,    Y, Y, N, N, N, X, MT_X,  N),
      CSRRC->     List(Y, N, N, N, CSR.C, A1_RS1,  A2_ZERO, IMM_X,  FN_ADD,    Y, Y, N, N, N, X, MT_X,  N),
      CSRRWI->    List(Y, N, N, N, CSR.W, A1_ZERO, A2_IMM,  IMM_Z,  FN_ADD,    Y, N, N, N, N, X, MT_X,  N),
      CSRRSI->    List(Y, N, N, N, CSR.S, A1_ZERO, A2_IMM,  IMM_Z,  FN_ADD,    Y, N, N, N, N, X, MT_X,  N),
      CSRRCI->    List(Y, N, N, N, CSR.C, A1_ZERO, A2_IMM,  IMM_Z,  FN_ADD,    Y, N, N, N, N, X, MT_X,  N))

  val decodeTable = intDecodeTable ++
    (if (haveMExt) mulDivDecodeTable else Array[(BitPat, List[BitPat])]())

  val cs = DecodeLogic(io.dpath.inst, decode_default, decodeTable)

  val (id_opcode_valid: Bool) :: (id_j: Bool) :: (id_br: Bool) :: cs1 = cs
  val (id_fence_i: Bool) :: _id_csr :: cs2 = cs1
  val id_sel_alu1 :: id_sel_alu2 :: id_sel_imm :: id_fn_alu :: (id_wen: Bool) :: (id_ren1: Bool) :: (id_ren2: Bool) :: cs3 = cs2
  val (id_set_sb: Bool) :: (id_mem_valid: Bool) :: (id_mem_rw: Bool) :: id_mem_type :: (id_mul_valid: Bool) :: Nil = cs3

  // ll stands for long-latency
  val ll_valid = Reg(init = Bool(false))
  val ll_waddr = Reg(UInt())
  val ll_fn = Reg(Bool()) // 0:mem, 1:mul/div
  val ll_mem_rw = Reg(Bool())
  val ll_mem_type = Reg(UInt())

  val id_waddr = io.dpath.inst(11, 7)
  val id_raddr1 = io.dpath.inst(19, 15)
  val id_raddr2 = io.dpath.inst(24, 20)
  val id_csr_en = _id_csr =/= CSR.N
  val id_csr_ren = (_id_csr === CSR.S || _id_csr === CSR.C) && id_raddr1 === UInt(0)
  val id_csr = Mux(id_csr_ren, CSR.R, _id_csr)

  val id_sb_stall = ll_valid
  val id_dmem_stall = io.dpath.id.mem_valid && !io.dmem.hready
  val id_mul_stall = io.dpath.id.mul_valid && !io.dpath.mul_ready

  val id_regs_valid =
    if (!haveEExt) Bool(true)
    else !((id_wen && id_waddr(4)) || (id_ren1 && id_raddr1(4)) || (id_ren2 && id_raddr2(4)))
  val id_inst_valid = id_opcode_valid && id_regs_valid
  val id_ok = !id_sb_stall && id_valid && id_inst_valid

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  val imem_bus_error = io.imem.hready && io.imem.hresp === HRESP_ERROR
  val dmem_bus_error = io.dmem.hready && io.dmem.hresp === HRESP_ERROR

  val (id_xcpt_nomem, id_cause_nomem) = checkExceptions(List(
    (io.dpath.csr_interrupt, io.dpath.csr_interrupt_cause),
    (io.dpath.ma_pc, UInt(Causes.misaligned_fetch)),
    (imem_bus_error, UInt(Causes.fault_fetch)),
    (!id_sb_stall && id_valid && !id_inst_valid, UInt(Causes.illegal_instruction)) ))

  val (id_xcpt, id_cause) = checkExceptions(List(
    (id_xcpt_nomem, id_cause_nomem),
    (id_ok && id_mem_valid && !id_mem_rw && io.dpath.ma_addr, UInt(Causes.misaligned_load)),
    (id_ok && id_mem_valid &&  id_mem_rw && io.dpath.ma_addr, UInt(Causes.misaligned_store)),
    (dmem_bus_error && !ll_mem_rw, UInt(Causes.fault_load)),
    (dmem_bus_error &&  ll_mem_rw, UInt(Causes.fault_store)) ))

  val id_retire_nomem_exclude_csr = id_ok && !id_xcpt_nomem
  val id_retire_nomem = id_retire_nomem_exclude_csr && !io.dpath.csr_xcpt
  val id_retire = id_ok && !id_xcpt && !io.dpath.csr_xcpt

  val id_imem_invalidate = id_retire_nomem && id_fence_i
  val id_br_taken = io.dpath.id.br && io.dpath.br_taken
  val id_redirect = io.dpath.id.j || id_br_taken || id_xcpt || io.dpath.csr_xcpt || io.dpath.csr_eret
  when (id_redirect && !io.imem.hready) { if_kill := Bool(true) }
  when (if_kill && io.imem.hready) { if_kill := Bool(false) }

  io.dpath.stallf := !id_redirect && (if_kill || !io.imem.hready || id_imem_invalidate || io.dpath.stalldx || io.dpath.stallw)
  io.dpath.killf := !io.imem.hready || if_kill || io.dpath.csr_interrupt || id_imem_invalidate || id_redirect
  io.dpath.stalldx := id_sb_stall || id_dmem_stall || id_mul_stall || io.dpath.stallw
  io.dpath.killdx := !id_retire || io.dpath.stalldx
  io.dpath.stallw := ll_valid && !ll_fn && !io.dmem.hready

  io.dpath.id.j := id_retire_nomem && id_j
  io.dpath.id.br := id_retire_nomem && id_br
  io.dpath.id.sel_alu1 := id_sel_alu1
  io.dpath.id.sel_alu2 := id_sel_alu2
  io.dpath.id.sel_imm := id_sel_imm
  io.dpath.id.fn_alu := id_fn_alu
  io.dpath.id.wen := id_retire && id_wen
  io.dpath.id.csr_en := id_retire_nomem && id_csr_en
  io.dpath.id.csr_cmd := Mux(id_retire_nomem_exclude_csr, id_csr, CSR.N)
  io.dpath.id.mem_valid := id_retire && id_mem_valid
  io.dpath.id.mem_rw := id_mem_rw
  io.dpath.id.mem_type := id_mem_type
  io.dpath.id.mul_valid := id_retire_nomem && id_mul_valid
  io.dpath.id.xcpt := id_xcpt
  io.dpath.id.cause := id_cause

  io.dpath.ll.valid := ll_valid
  io.dpath.ll.waddr := ll_waddr
  io.dpath.ll.fn := ll_fn
  io.dpath.ll.mem_rw := ll_mem_rw
  io.dpath.ll.mem_type := ll_mem_type

  io.dmem.hburst := HBURST_SINGLE
  io.dmem.hprot := UInt("b0011")
  io.dmem.hmastlock := Bool(false)
  io.dmem.htrans := Mux(io.dpath.id.mem_valid, HTRANS_NONSEQ, HTRANS_IDLE)

  // have to clear sb first before setting, because id_sb_stall is bypassed
  when (io.dpath.clear_sb) {
    ll_valid := Bool(false)
  }

  when (!io.dpath.killdx) {
    when (id_set_sb) {
      ll_valid := Bool(true)
      ll_waddr := id_waddr
      when (id_mem_valid) { ll_fn := Bool(false) }
      when (id_mul_valid) { ll_fn := Bool(true) }
    }
    when (id_mem_valid) {
      ll_mem_rw := id_mem_rw
      ll_mem_type := id_mem_type
    }
  }

  io.dpath.logging.invalidate := id_imem_invalidate
  io.dpath.logging.sb_stall := id_sb_stall
  io.dpath.logging.dmem_stall := id_dmem_stall
  io.dpath.logging.mul_stall := id_mul_stall
}
