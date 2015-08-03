// See LICENSE for license details.

package zscale

import Chisel._
import junctions._
import uncore._
import rocket._
import rocket.Util._
import rocket.ALU._

// copied and modified from Rocket's datapath
class ALU extends Module with ZscaleParameters
{
  val io = new Bundle {
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in1 = Bits(INPUT, xprLen)
    val in2 = Bits(INPUT, xprLen)
    val out = Bits(OUTPUT, xprLen)
    val adder_out = Bits(OUTPUT, xprLen)
  }

  // ADD, SUB
  val sum = io.in1 + Mux(isSub(io.fn), -io.in2, io.in2)

  // SLT, SLTU
  val cmp = cmpInverted(io.fn) ^
    Mux(cmpEq(io.fn), sum === UInt(0),
    Mux(io.in1(xprLen-1) === io.in2(xprLen-1), sum(xprLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xprLen-1), io.in1(xprLen-1))))

  // SLL, SRL, SRA
  val shamt = io.in2(4,0)
  val shin_r = io.in1
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xprLen-1), shin).toSInt >> shamt)(xprLen-1,0)
  val shout_l = Reverse(shout_r)

  io.out :=
    Mux(io.fn === FN_ADD || io.fn === FN_SUB,  sum,
    Mux(io.fn === FN_SR  || io.fn === FN_SRA,  shout_r,
    Mux(io.fn === FN_SL,                       shout_l,
    Mux(io.fn === FN_AND,                      io.in1 & io.in2,
    Mux(io.fn === FN_OR,                       io.in1 | io.in2,
    Mux(io.fn === FN_XOR,                      io.in1 ^ io.in2,
                /* all comparisons */          cmp))))))

  io.adder_out := sum
}

class Datapath extends Module with ZscaleParameters
{
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip
    val imem = new HASTIMasterIO
    val dmem = new HASTIMasterIO
    val host = new HTIFIO
  }

  val npc = UInt()
  val pc = Reg(init = UInt("h1fc", xprLen))
  val id_br_target = UInt()
  val csr = Module(new rocket.CSRFile, {
    case UseVM => false
    case XLen => 32
    case BuildFPU => None
  })
  val xcpt = io.ctrl.id.xcpt || io.ctrl.csr_xcpt

  npc := (Mux(io.ctrl.id.j || io.ctrl.id.br && io.ctrl.br_taken, id_br_target,
          Mux(xcpt || io.ctrl.csr_eret, csr.io.evec,
              pc + UInt(4))).toSInt & SInt(-2)).toUInt

  when (!io.ctrl.stallf) {
    pc := npc
  }

  io.imem.haddr := Mux(io.ctrl.stallf, pc, npc)

  val id_pc = Reg(UInt(width = xprLen))
  val id_inst = Reg(Bits(width = coreInstBits))

  val wb_wen = Reg(init = Bool(false))
  val wb_waddr = Reg(UInt())
  val wb_wdata = Reg(Bits())

  // !io.ctrl.killf is a power optimization (clock-gating)
  when (!io.ctrl.stalldx && !io.ctrl.killf) {
    id_pc := pc
    id_inst := io.imem.hrdata
  }

  // copied from Rocket's datapath
  class RegFile {
    private val rf = Mem(Bits(width = xprLen), 31)
    private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
    private var canRead = true
    def read(addr: UInt) = {
      require(canRead)
      reads += addr -> UInt()
      reads.last._2 := Mux(addr != UInt(0), rf(~addr), Bits(0))
      reads.last._2
    }
    def write(addr: UInt, data: UInt) = {
      canRead = false
      when (addr != UInt(0)) {
        rf(~addr) := data
        for ((raddr, rdata) <- reads)
          when (addr === raddr) { rdata := data }
      }
    }
  }

  // copied from Rocket's datapath
  def imm(sel: UInt, inst: UInt) = {
    val sign = inst(31).toSInt
    val b30_20 = Mux(sel === IMM_U, inst(30,20).toSInt, sign)
    val b19_12 = Mux(sel != IMM_U && sel != IMM_UJ, sign, inst(19,12).toSInt)
    val b11 = Mux(sel === IMM_U || sel === IMM_Z, SInt(0),
              Mux(sel === IMM_UJ, inst(20).toSInt,
              Mux(sel === IMM_SB, inst(7).toSInt, sign)))
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, Bits(0), inst(30,25))
    val b4_1 = Mux(sel === IMM_U, Bits(0),
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8),
               Mux(sel === IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === IMM_S, inst(7),
             Mux(sel === IMM_I, inst(20),
             Mux(sel === IMM_Z, inst(15), Bits(0))))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).toSInt
  }

  val rf = new RegFile
  val id_addr = Vec(id_inst(19, 15), id_inst(24,20))
  val id_rs = id_addr.map(rf.read _)
  val id_rd = id_inst(11, 7)
  val id_imm = imm(io.ctrl.id.sel_imm, id_inst)

  // ALU
  val alu = Module(new ALU)
  alu.io.fn := io.ctrl.id.fn_alu
  alu.io.in1 := MuxLookup(io.ctrl.id.sel_alu1, SInt(0), Seq(
      A1_RS1 -> id_rs(0).toSInt,
      A1_PC -> id_pc
    ))
  alu.io.in2 := MuxLookup(io.ctrl.id.sel_alu2, SInt(0), Seq(
      A2_FOUR -> SInt(4),
      A2_RS2 -> id_rs(1).toSInt,
      A2_IMM -> id_imm
    ))

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
  class StoreGen32(typ: UInt, addr: UInt, dat: UInt) {
    val byte = typ === MT_B || typ === MT_BU
    val half = typ === MT_H || typ === MT_HU
    val word = typ === MT_W
    def size =
      Mux(byte, UInt("b000"),
      Mux(half, UInt("b001"),
                UInt("b010")))
    def data =
      Mux(byte, Fill(4, dat( 7,0)),
      Mux(half, Fill(2, dat(15,0)),
                Fill(1, dat(31,0))))
  }

  class LoadGen32(typ: UInt, addr: UInt, dat: UInt) {
    val t = new StoreGen32(typ, addr, dat)
    val sign = typ === MT_B || typ === MT_H || typ === MT_W

    val word = dat
    val halfShift = Mux(addr(1), word(31,16), word(15,0))
    val half = Cat(Mux(t.half, Fill(16, sign && halfShift(15)), word(31,16)), halfShift)
    val byteShift = Mux(addr(0), half(15,8), half(7,0))
    val byte = Cat(Mux(t.byte, Fill(24, sign && byteShift(7)), half(31,8)), byteShift)
  }

  val dmem_req_addr = alu.io.adder_out
  val dmem_sgen = new StoreGen32(io.ctrl.id.mem_type, dmem_req_addr, id_rs(1))
  val dmem_load_lowaddr = RegEnable(dmem_req_addr(1, 0), io.ctrl.id.mem_valid && !io.ctrl.id.mem_rw)
  when (io.ctrl.id.mem_valid && io.ctrl.id.mem_rw) { wb_wdata := dmem_sgen.data } // share wb_wdata with store data

  io.dmem.haddr := dmem_req_addr
  io.dmem.hwrite := io.ctrl.id.mem_rw
  io.dmem.hsize := dmem_sgen.size
  io.dmem.hwdata := wb_wdata

  val dmem_clear_sb = io.ctrl.ll.valid && !io.ctrl.ll.fn && io.dmem.hready
  val dmem_resp_valid = dmem_clear_sb && !io.ctrl.ll.mem_rw
  val dmem_lgen = new LoadGen32(io.ctrl.ll.mem_type, dmem_load_lowaddr, io.dmem.hrdata)

  // MUL/DIV
  val muldiv = Module(new MulDiv(
      mulUnroll = if(params(FastMulDiv)) 8 else 1,
      earlyOut = params(FastMulDiv)), { case XLen => 32 })
  muldiv.io.req.valid := io.ctrl.id.mul_valid
  muldiv.io.req.bits.fn := io.ctrl.id.fn_alu
  muldiv.io.req.bits.dw := DW_64
  muldiv.io.req.bits.in1 := id_rs(0)
  muldiv.io.req.bits.in2 := id_rs(1)
  muldiv.io.kill := Bool(false)
  muldiv.io.resp.ready := Bool(true)

  // WB
  val wen = io.ctrl.id.wen || dmem_resp_valid || muldiv.io.resp.valid
  val waddr = Mux(dmem_resp_valid || muldiv.io.resp.valid, io.ctrl.ll.waddr, id_rd)
  val wdata = MuxCase(
    alu.io.out, Array(
      io.ctrl.id.csr_en -> csr.io.rw.rdata,
      dmem_resp_valid -> dmem_lgen.byte,
      muldiv.io.resp.valid -> muldiv.io.resp.bits.data
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
  io.ctrl.ma_addr := (dmem_req_addr(1) || dmem_req_addr(0)) && dmem_sgen.word || dmem_req_addr(0) && dmem_sgen.half
  io.ctrl.br_taken := alu.io.out(0)
  io.ctrl.mul_ready := muldiv.io.req.ready
  io.ctrl.clear_sb := dmem_clear_sb || muldiv.io.resp.valid
  io.ctrl.csr_replay := csr.io.csr_replay
  io.ctrl.csr_xcpt := csr.io.csr_xcpt
  io.ctrl.csr_eret := csr.io.eret
  io.ctrl.csr_interrupt := csr.io.interrupt
  io.ctrl.csr_interrupt_cause := csr.io.interrupt_cause

  printf("Z%d: %d [%d] [%s%s%s%s%s%s|%s%s%s%s%s] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] [%d|%x] inst=[%x] DASM(%x)\n",
    io.host.id, csr.io.time(31, 0), !io.ctrl.killdx,
    Reg(init=45,next=Mux(!io.imem.hready, 73, 45)), // I -
    Reg(init=45,next=Mux(io.ctrl.id.br && io.ctrl.br_taken, 66, 45)), // B -
    Reg(init=45,next=Mux(io.ctrl.id.j, 74, 45)), // J -
    Reg(init=45,next=Mux(io.ctrl.logging.invalidate, 86, 45)), // V -
    Reg(init=45,next=Mux(io.ctrl.csr_eret, 83, 45)), // S -
    Reg(init=45,next=Mux(xcpt, 88, 45)), // X -
    Mux(io.ctrl.logging.sb_stall, 83, 45), // S -
    Mux(io.ctrl.logging.scr_stall, 67, 45), // C -
    Mux(io.ctrl.logging.dmem_stall, 68, 45), // D -
    Mux(io.ctrl.logging.mul_stall, 77, 45), // M -
    Mux(xcpt, 88, 45), // X -
    id_pc, waddr, wdata, wen, id_addr(0), id_rs(0), id_addr(1), id_rs(1),
    xcpt, io.ctrl.id.cause,
    id_inst, id_inst)
}
