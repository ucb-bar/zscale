// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._
import rocket.Util._
import rocket.ALU._

// copied and modified from Rocket's datapath
class ALU extends Module with ZScaleParameters
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

class Datapath extends Module with ZScaleParameters
{
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip
    val imem = new InstMemIO
    val dmem = new ScratchPadIO
    val host = new HTIFIO
    val scr = new SCRIO
  }

  val pc = Reg(init = UInt(0, xprLen))
  val id_br_target = UInt()
  val csr = Module(new CSRFile)

  when (!io.ctrl.stallf) {
    pc := Mux(io.ctrl.j || io.ctrl.br && io.ctrl.br_taken, id_br_target,
          Mux(io.ctrl.xcpt || io.ctrl.sret, csr.io.evec,
              pc + UInt(4))) & SInt(-2)
  }

  io.imem.req.bits.addr := pc

  val id_pc = Reg(UInt(width = xprLen))
  val id_inst = Reg(Bits(width = coreInstBits))

  // !io.ctrl.killf is a power optimization (clock-gating)
  when (!io.ctrl.stalldx && !io.ctrl.killf) {
    id_pc := pc
    id_inst := io.imem.resp.bits.inst
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
  def imm(sel: Bits, inst: Bits) = {
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
  val id_imm = imm(io.ctrl.sel_imm, id_inst)

  // ALU
  val alu = Module(new ALU)
  alu.io.fn := io.ctrl.fn_alu
  alu.io.in1 := MuxLookup(io.ctrl.sel_alu1, SInt(0), Seq(
      A1_RS1 -> id_rs(0).toSInt,
      A1_PC -> id_pc
    ))
  alu.io.in2 := MuxLookup(io.ctrl.sel_alu2, SInt(0), Seq(
      A2_FOUR -> SInt(4),
      A2_RS2 -> id_rs(1).toSInt,
      A2_IMM -> id_imm
    ))

  // BRANCH TARGET
  // jalr only takes rs1, jump and branches take pc
  id_br_target := Mux(io.ctrl.j && io.ctrl.sel_imm === IMM_I, id_rs(0), id_pc) + id_imm

  // CSR
  val csr_operand = alu.io.adder_out
  csr.io.host <> io.host
  csr.io.rw.addr := id_inst(31, 20)
  csr.io.rw.cmd := io.ctrl.csr_cmd
  csr.io.rw.wdata :=
    Mux(io.ctrl.csr_cmd === CSR.S, csr.io.rw.rdata | csr_operand,
    Mux(io.ctrl.csr_cmd === CSR.C, csr.io.rw.rdata & ~csr_operand,
        csr_operand))

  io.scr <> csr.io.scr
  csr.io.masked_wen := id_addr(0) != UInt(0) || !Vec(CSR.S, CSR.C).contains(io.ctrl.csr_cmd)
  csr.io.retire := !io.ctrl.killdx
  csr.io.xcpt := io.ctrl.xcpt
  csr.io.cause := io.ctrl.cause
  csr.io.pc := id_pc
  csr.io.sret := io.ctrl.sret

  // DMEM
  class StoreGen32(typ: Bits, addr: Bits, dat: Bits) {
    val byte = typ === MT_B || typ === MT_BU
    val half = typ === MT_H || typ === MT_HU
    val word = typ === MT_W
    val nw = log2Up(spadWordBytes)
    def mask =
      Mux(byte, Bits(  1) <<     addr(nw-1,0),
      Mux(half, Bits(  3) << Cat(addr(nw-1,1), Bits(0,1)),
      Mux(word, Bits( 15) << Cat(addr(nw-1,2), Bits(0,2)),
                Fill(nw, Bool(true)))))
    def data =
      Mux(byte, Fill(nw*4, dat( 7,0)),
      Mux(half, Fill(nw*2, dat(15,0)),
                Fill(nw,   dat(31,0))))
  }

  class LoadGen32(typ: Bits, addr: Bits, dat: Bits) {
    val t = new StoreGen32(typ, addr, dat)
    val sign = typ === MT_B || typ === MT_H || typ === MT_W

    val word = MuxLookup(addr(t.nw-1,2), Bits(0, 32),
      (0 until spadWordBytes/4).map( i => (UInt(i) -> dat(32*(i+1)-1, 32*i)) ))
    val halfShift = Mux(addr(1), word(31,16), word(15,0))
    val half = Cat(Mux(t.half, Fill(16, sign && halfShift(15)), word(31,16)), halfShift)
    val byteShift = Mux(addr(0), half(15,8), half(7,0))
    val byte = Cat(Mux(t.byte, Fill(24, sign && byteShift(7)), half(31,8)), byteShift)
  }

  val dmem_req_addr = alu.io.adder_out
  val dmem_sgen = new StoreGen32(io.ctrl.mem_type, dmem_req_addr, id_rs(1))

  io.dmem.req.bits.addr := dmem_req_addr >> UInt(log2Up(spadWordBytes))
  io.dmem.req.bits.rw := io.ctrl.mem_rw
  io.dmem.req.bits.wmask := dmem_sgen.mask
  io.dmem.req.bits.data := dmem_sgen.data

  // we can register load metadata on the CPU side, since there's only one load in flight
  val dmem_load_valid = io.ctrl.mem_valid && !io.ctrl.mem_rw
  val dmem_load_mem_type = RegEnable(io.ctrl.mem_type, dmem_load_valid)
  val dmem_load_lowaddr = RegEnable(dmem_req_addr(log2Up(spadWordBytes)-1, 0), dmem_load_valid)
  val dmem_load_rd = RegEnable(id_rd, dmem_load_valid)

  val dmem_resp_valid = ShiftRegister(io.dmem.resp.valid, dmemRespStages, Bool(true))
  val dmem_resp_data = ShiftRegister(io.dmem.resp.bits.data, dmemRespStages, io.dmem.resp.valid)
  val dmem_lgen = new LoadGen32(dmem_load_mem_type, dmem_load_lowaddr, dmem_resp_data)

  // MUL/DIV
  val muldiv = Module(new MulDiv, {case XprLen => 32})
  muldiv.io.req.valid := io.ctrl.mul_valid
  muldiv.io.req.bits.fn := io.ctrl.fn_alu
  muldiv.io.req.bits.dw := DW_64
  muldiv.io.req.bits.in1 := id_rs(0)
  muldiv.io.req.bits.in2 := id_rs(1)
  muldiv.io.req.bits.tag := id_rd
  muldiv.io.kill := Bool(false)
  muldiv.io.resp.ready := Bool(true)

  // WB
  val wen = io.ctrl.wen || dmem_resp_valid || muldiv.io.resp.valid
  val waddr = MuxCase(
    id_rd, Array(
      dmem_resp_valid -> dmem_load_rd,
      muldiv.io.resp.valid -> muldiv.io.resp.bits.tag
    ))
  val wdata = MuxCase(
    alu.io.out, Array(
      io.ctrl.csr_en -> csr.io.rw.rdata,
      dmem_resp_valid -> dmem_lgen.byte,
      muldiv.io.resp.valid -> muldiv.io.resp.bits.data
    ))

  val reg_wen = Reg(init = Bool(false))
  val reg_waddr = Reg(UInt())
  val reg_wdata = Reg(Bits())

  reg_wen := wen
  when (wen) {
    reg_waddr := waddr
    reg_wdata := wdata
  }

  when (reg_wen) {
    rf.write(reg_waddr, reg_wdata)
  }

  // to control
  io.ctrl.status := csr.io.status
  io.ctrl.inst := id_inst
  io.ctrl.ma_pc := pc(1)
  io.ctrl.fa_pc := pc(xprLen-1, log2Up(spadSize)).orR
  io.ctrl.ma_addr := (dmem_req_addr(1) || dmem_req_addr(0)) && dmem_sgen.word || dmem_req_addr(0) && dmem_sgen.half
  io.ctrl.fa_addr := dmem_req_addr(xprLen-1, log2Up(spadSize)).orR
  io.ctrl.br_taken := alu.io.out(0)
  io.ctrl.mul_ready := muldiv.io.req.ready
  io.ctrl.clear_sb := dmem_resp_valid || muldiv.io.resp.valid

  printf("Z%d: %d [%d] [%s%s%s%s%s%s|%s%s%s%s%s%s] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] [%d|%x] inst=[%x] DASM(%x)\n",
    io.host.id, csr.io.time(31, 0), !io.ctrl.killdx,
    Reg(init=45,next=Mux(!io.imem.resp.valid, 73, 45)), // I -
    Reg(init=45,next=Mux(io.ctrl.br && io.ctrl.br_taken, 66, 45)), // B -
    Reg(init=45,next=Mux(io.ctrl.j, 74, 45)), // J -
    Reg(init=45,next=Mux(io.ctrl.invalidate, 86, 45)), // V -
    Reg(init=45,next=Mux(io.ctrl.sret, 83, 45)), // S -
    Reg(init=45,next=Mux(io.ctrl.xcpt, 88, 45)), // X -
    Mux(io.ctrl.sb_stall, 83, 45), // S -
    Mux(io.ctrl.scr_stall, 67, 45), // C -
    Mux(io.ctrl.imem_stall, 73, 45), // I -
    Mux(io.ctrl.dmem_stall, 68, 45), // D -
    Mux(io.ctrl.mul_stall, 77, 45), // M -
    Mux(io.ctrl.xcpt, 88, 45), // X -
    id_pc, waddr, wdata, wen, id_addr(0), id_rs(0), id_addr(1), id_rs(1),
    io.ctrl.xcpt, io.ctrl.cause,
    id_inst, id_inst)
}
