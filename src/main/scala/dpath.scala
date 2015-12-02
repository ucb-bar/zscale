// See LICENSE for license details.

package zscale

import Chisel._
import cde.Parameters
import junctions._
import uncore._
import rocket._
import rocket.Util._
import rocket.ALU._

class Datapath(implicit p: Parameters) extends ZscaleModule()(p) {
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip
    val imem = new HastiMasterIO
    val dmem = new HastiMasterIO
    val host = new HtifIO
  }

  val pc = Reg(init = UInt("h1fc", xLen))
  val id_br_target = Wire(UInt())
  val csr = Module(new rocket.CSRFile()(p.alterPartial({
    case UseVM => false
    case XLen => 32
    case UseFPU => false
  })))
  val xcpt = io.ctrl.id.xcpt || io.ctrl.csr_xcpt

  val npc = (Mux(io.ctrl.id.j || io.ctrl.id.br && io.ctrl.br_taken, id_br_target,
             Mux(xcpt || io.ctrl.csr_eret, csr.io.evec,
                 pc + UInt(4))).toSInt & SInt(-2)).toUInt

  when (!io.ctrl.stallf) {
    pc := npc
  }

  io.imem.haddr := Mux(io.ctrl.stallf, pc, npc)

  val id_pc = Reg(UInt(width = xLen))
  val id_inst = Reg(Bits(width = coreInstBits))

  val wb_wen = Reg(init = Bool(false))
  val wb_waddr = Reg(UInt())
  val wb_wdata = Reg(Bits())

  // !io.ctrl.killf is a power optimization (clock-gating)
  when (!io.ctrl.stalldx && !io.ctrl.killf) {
    id_pc := pc
    id_inst := io.imem.hrdata
  }

  val rf = new RegFile(if (haveEExt) 15 else 31, 32, true)
  val id_addr = Vec(id_inst(19, 15), id_inst(24,20))
  val id_rs = id_addr.map(rf.read _)
  val id_rd = id_inst(11, 7)
  val id_imm = ImmGen(io.ctrl.id.sel_imm, id_inst)

  // ALU
  val alu = Module(new ALU)
  alu.io.fn := io.ctrl.id.fn_alu
  alu.io.in1 := MuxLookup(io.ctrl.id.sel_alu1, SInt(0), Seq(
      A1_RS1 -> id_rs(0).toSInt,
      A1_PC -> id_pc.toSInt
    )).toUInt
  alu.io.in2 := MuxLookup(io.ctrl.id.sel_alu2, SInt(0), Seq(
      A2_FOUR -> SInt(4),
      A2_RS2 -> id_rs(1).toSInt,
      A2_IMM -> id_imm
    )).toUInt

  // BRANCH TARGET
  // jalr only takes rs1, jump and branches take pc
  id_br_target := (Mux(io.ctrl.id.j && io.ctrl.id.sel_imm === IMM_I, id_rs(0), id_pc).toSInt + id_imm).toUInt

  // CSR
  val csr_operand = alu.io.adder_out
  csr.io.host <> io.host
  csr.io.rw.addr := id_inst(31, 20)
  csr.io.rw.cmd := io.ctrl.id.csr_cmd
  csr.io.rw.wdata := csr_operand

  csr.io.exception := io.ctrl.id.xcpt
  csr.io.retire := !io.ctrl.killdx
  csr.io.cause := io.ctrl.id.cause
  csr.io.pc := id_pc

  // DMEM
  val dmem_req_addr = alu.io.adder_out
  val dmem_sgen = new StoreGen(io.ctrl.id.mem_type, dmem_req_addr, id_rs(1), 4)
  val dmem_load_lowaddr = RegEnable(dmem_req_addr(1, 0), io.ctrl.id.mem_valid && !io.ctrl.id.mem_rw)
  when (io.ctrl.id.mem_valid && io.ctrl.id.mem_rw) { wb_wdata := dmem_sgen.data } // share wb_wdata with store data

  io.dmem.haddr := dmem_req_addr
  io.dmem.hwrite := io.ctrl.id.mem_rw
  io.dmem.hsize := dmem_sgen.size
  io.dmem.hwdata := wb_wdata

  val dmem_clear_sb = io.ctrl.ll.valid && !io.ctrl.ll.fn && io.dmem.hready
  val dmem_resp_valid = dmem_clear_sb && !io.ctrl.ll.mem_rw
  val dmem_lgen = new LoadGen(io.ctrl.ll.mem_type, dmem_load_lowaddr, io.dmem.hrdata, Bool(false), 4)

  // MUL/DIV
  val (mulDivRespValid, mulDivRespData, mulDivReqReady) = if (haveMExt) {
    val muldiv = Module(new MulDiv(width = xLen,
                                   unroll = if(fastMulDiv) 8 else 1,
                                   earlyOut = fastMulDiv))
    muldiv.io.req.valid := io.ctrl.id.mul_valid
    muldiv.io.req.bits.fn := io.ctrl.id.fn_alu
    muldiv.io.req.bits.dw := DW_64
    muldiv.io.req.bits.in1 := id_rs(0)
    muldiv.io.req.bits.in2 := id_rs(1)
    muldiv.io.kill := Bool(false)
    muldiv.io.resp.ready := Bool(true)
    (muldiv.io.resp.valid, muldiv.io.resp.bits.data, muldiv.io.req.ready)
  } else (Bool(false), UInt(0), Bool(false))

  // WB
  val ll_wen = dmem_resp_valid || mulDivRespValid
  val wen = io.ctrl.id.wen || ll_wen
  val waddr = Mux(ll_wen, io.ctrl.ll.waddr, id_rd)
  val wdata = MuxCase(
    alu.io.out, Array(
      io.ctrl.id.csr_en -> csr.io.rw.rdata,
      dmem_resp_valid -> dmem_lgen.data,
      mulDivRespValid -> mulDivRespData
    ))

  wb_wen := wen
  when (wen) {
    wb_waddr := waddr
    wb_wdata := wdata
  }

  when (wb_wen) {
    rf.write(wb_waddr, wb_wdata)
  }

  // to control
  io.ctrl.inst := id_inst
  io.ctrl.ma_pc := pc(1)
  io.ctrl.ma_addr := dmem_sgen.misaligned
  io.ctrl.br_taken := alu.io.out(0)
  io.ctrl.mul_ready := mulDivReqReady
  io.ctrl.clear_sb := dmem_clear_sb || mulDivRespValid
  io.ctrl.csr_xcpt := csr.io.csr_xcpt
  io.ctrl.csr_eret := csr.io.eret
  io.ctrl.csr_interrupt := csr.io.interrupt
  io.ctrl.csr_interrupt_cause := csr.io.interrupt_cause

  printf("Z%d: %d [%d] [%s%s%s%s%s%s|%s%s%s%s] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] [%d|%x] inst=[%x] DASM(%x)\n",
    io.host.id, csr.io.time(31, 0), !io.ctrl.killdx,
    Reg(init=45,next=Mux(!io.imem.hready, 73, 45)), // I -
    Reg(init=45,next=Mux(io.ctrl.id.br && io.ctrl.br_taken, 66, 45)), // B -
    Reg(init=45,next=Mux(io.ctrl.id.j, 74, 45)), // J -
    Reg(init=45,next=Mux(io.ctrl.logging.invalidate, 86, 45)), // V -
    Reg(init=45,next=Mux(io.ctrl.csr_eret, 83, 45)), // S -
    Reg(init=45,next=Mux(xcpt, 88, 45)), // X -
    Mux(io.ctrl.logging.sb_stall, 83, 45), // S -
    Mux(io.ctrl.logging.dmem_stall, 68, 45), // D -
    Mux(io.ctrl.logging.mul_stall, 77, 45), // M -
    Mux(xcpt, 88, 45), // X -
    id_pc, waddr, wdata, wen, id_addr(0), id_rs(0), id_addr(1), id_rs(1),
    xcpt, io.ctrl.id.cause,
    id_inst, id_inst)
}
