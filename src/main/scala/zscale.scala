// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._
import rocket.Util._

abstract trait PCUParameters extends UsesParameters
{
  val memAddrBits = params(MIFAddrBits)
  val memDataBits = params(MIFDataBits)
  val memDataBeats = params(MIFDataBeats)

  val spadSize = 8192
  val spadWidth = memDataBits
  val spadWordBytes = spadWidth / 8
  val spadDepth = spadSize / spadWordBytes
  val spadByteMaskBits = spadWordBytes
  val spadAddrBits = log2Up(spadDepth)

  val xprLen = 32
  val addrBits = log2Up(spadSize)
  val coreInstBits = params(CoreInstBits) 
  val coreInstBytes = coreInstBits / 8
}

class MemIOSplitter extends Module with PCUParameters
{
  val io = new Bundle {
    val hub = new MemPipeIO().flip
    val serdes = new MemPipeIO
    val spad = new MemPipeIO
  }

  val addr_msb = io.hub.req_cmd.bits.addr(memAddrBits-1)
  val asid_serdes = addr_msb === UInt(0)

  io.serdes.req_cmd.valid := io.hub.req_cmd.valid && asid_serdes
  io.spad.req_cmd.valid := io.hub.req_cmd.valid && !asid_serdes
  io.hub.req_cmd.ready := asid_serdes && io.serdes.req_cmd.ready || !asid_serdes && io.spad.req_cmd.ready
  io.serdes.req_cmd.bits := io.hub.req_cmd.bits
  io.spad.req_cmd.bits := io.hub.req_cmd.bits

  io.serdes.req_data.valid := io.hub.req_data.valid
  io.spad.req_data.valid := io.hub.req_data.valid
  io.hub.req_data.ready := io.serdes.req_data.ready || io.spad.req_data.ready
  io.serdes.req_data.bits := io.hub.req_data.bits
  io.spad.req_data.bits := io.hub.req_data.bits

  io.hub.resp.valid := io.serdes.resp.valid || io.spad.resp.valid
  io.hub.resp.bits := Mux(io.spad.resp.valid, io.spad.resp.bits, io.serdes.resp.bits)
}

class ScratchPadRequest extends Bundle with PCUParameters
{
  val addr = UInt(width = spadAddrBits)
  val rw = Bool()
  val wmask = Bits(width = spadByteMaskBits)
  val data = Bits(width = spadWidth)
}

class ScratchPadResponse extends Bundle with PCUParameters
{
  val data = Bits(width = spadWidth)
}

class ScratchPadIO extends Bundle with PCUParameters
{
  val req = Decoupled(new ScratchPadRequest)
  val resp = Valid(new ScratchPadResponse).flip
}

class ScratchPad extends Module with PCUParameters
{
  val io = new Bundle {
    val cpu = new ScratchPadIO().flip
    val mem = new MemPipeIO().flip
  }

  val s_idle :: s_read :: s_write :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  val ram = Mem(Bits(width = spadWidth), spadDepth, seqRead = true)
  val raddr = Reg(UInt())
  val rdata = Bits()
  val wen = Bool()
  val waddr = UInt()
  val wdata = Bits()
  val wmask = Bits()

  // ram read port
  rdata := ram(raddr)

  // ram write port
  when (wen) {
    ram.write(waddr, wdata, wmask)
  }

  wen := Bool(false)
  waddr := io.cpu.req.bits.addr
  wdata := io.cpu.req.bits.data
  wmask := FillInterleaved(8, io.cpu.req.bits.wmask)

  io.cpu.req.ready := (state === s_idle)

  when (state === s_idle && io.cpu.req.valid) {
    when (io.cpu.req.bits.rw) {
      wen := Bool(true)
    } .otherwise {
      raddr := io.cpu.req.bits.addr
    }
  }

  io.cpu.resp.valid := Reg(next = io.cpu.req.fire() && !io.cpu.req.bits.rw)
  io.cpu.resp.bits.data := rdata

  val addr = Reg(UInt())
  val tag = Reg(UInt())
  val cnt = Reg(UInt(width = log2Up(memDataBeats)))

  when (state === s_idle && io.mem.req_cmd.valid) {
    state := Mux(io.mem.req_cmd.bits.rw, s_write, s_read)
    addr := io.mem.req_cmd.bits.addr
    tag := io.mem.req_cmd.bits.tag
    cnt := UInt(0)
  }
  when (state === s_read) {
    cnt := cnt + UInt(1)
    when (cnt === UInt(memDataBeats-1)) {
      state := s_idle
    }
  }
  when (state === s_write && io.mem.req_data.valid) {
    cnt := cnt + UInt(1)
    when (cnt === UInt(memDataBeats-1)) {
      state := s_idle
    }
  }

  io.mem.req_cmd.ready := (state === s_idle)
  io.mem.req_data.ready := (state === s_write)

  when (state === s_read) {
    raddr := Cat(addr, cnt)
  }
  when (io.mem.req_data.fire()) {
    wen := Bool(true)
    waddr := Cat(addr, cnt)
    wdata := io.mem.req_data.bits.data
    wmask := FillInterleaved(spadWidth, Bool(true))
  }

  io.mem.resp.valid := Reg(next = state === s_read)
  io.mem.resp.bits.data := rdata
  io.mem.resp.bits.tag := tag
}

class InstMemReq extends Bundle with PCUParameters
{
  val addr = UInt(width = addrBits)
}

class InstMemResp extends Bundle with PCUParameters
{
  val inst = Bits(width = coreInstBits)
}

class InstMemIO extends Bundle with PCUParameters
{
  val req = Valid(new InstMemReq)
  val resp = Valid(new InstMemResp).flip
}

class InstLineBuffer extends Module with PCUParameters
{
  val io = new Bundle {
    val cpu = new InstMemIO().flip
    val mem = new ScratchPadIO
  }

  val s_idle :: s_requested :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val nInst = spadWordBytes / coreInstBytes
  val nIdx = log2Up(nInst) + log2Up(coreInstBytes)
  val nTag = addrBits - nIdx

  val req_inst_onehot = UInt(1) << io.cpu.req.bits.addr(nIdx-1, log2Up(coreInstBytes))
  val req_tag = io.cpu.req.bits.addr(addrBits-1, nIdx)

  val tag = Reg(UInt(width = nTag))
  val tag_valid = Reg(init = Bool(false))
  val tag_hit = tag_valid && (tag === req_tag)

  val service_hit = (state === s_idle) && tag_hit
  val service_nohit = (state === s_idle) && !tag_hit

  val line = Vec.fill(nInst){Reg(Bits(width = coreInstBits))}

  // front side of line buffer
  io.cpu.resp.valid := service_hit
  io.cpu.resp.bits.inst := Mux1H(req_inst_onehot, line)

  // back side of line buffer
  when (service_nohit && io.cpu.req.valid && io.mem.req.ready) {
    state := s_requested
    tag_valid := Bool(false)
    tag := req_tag
  }

  io.mem.req.valid := !this.reset && service_nohit && io.cpu.req.valid
  io.mem.req.bits.addr := io.cpu.req.bits.addr >> UInt(nIdx)
  io.mem.req.bits.rw := Bool(false)

  when (io.mem.resp.valid) {
    state := s_idle
    tag_valid := Bool(true)
    (0 until nInst).map(i => line(i) := io.mem.resp.bits.data((i+1)*coreInstBits-1, i*coreInstBits))
  }
}

class CtrlDpathIO extends Bundle with PCUParameters
{
  val stallf = Bool(OUTPUT)
  val killf = Bool(OUTPUT)
  val stalldx = Bool(OUTPUT)
  val killdx = Bool(OUTPUT)
  val wen = Bool(OUTPUT)

  val inst = Bits(INPUT, coreInstBits)
}

class Control extends Module with PCUParameters
{
  val io = new Bundle {
    val dpath = new CtrlDpathIO
    val imem = new InstMemIO
  }

  io.imem.req.valid := Bool(true)

  val id_valid = Reg(init = Bool(false))

  id_valid := io.imem.resp.valid

  io.dpath.stallf := !io.imem.resp.valid || io.dpath.stalldx
  io.dpath.killf := io.dpath.stallf
  io.dpath.stalldx := Bool(false)
  io.dpath.killdx := !id_valid || io.dpath.stalldx
  io.dpath.wen := !io.dpath.killdx
}

class ALU extends Module with PCUParameters
{
  val io = new Bundle {
    val in1 = Bits(INPUT, xprLen)
    val in2 = Bits(INPUT, xprLen)
    val out = Bits(OUTPUT, xprLen)
  }

  io.out := io.in1 + io.in2
}

class Datapath extends Module with PCUParameters
{
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip
    val imem = new InstMemIO
  }

  val pc = Reg(init = UInt(0, addrBits))

  when (!io.ctrl.stallf) {
    pc := pc + UInt(4)
  }

  io.imem.req.bits.addr := pc

  val id_pc = Reg(UInt(width = addrBits))
  val id_inst = Reg(Bits(width = coreInstBits))

  when (!io.ctrl.killf) {
    id_pc := pc
    id_inst := io.imem.resp.bits.inst
  }

  class RegFile {
    private val rf = Mem(Bits(width = xprLen), 31)
    def read(addr: UInt) = Mux(addr != UInt(0), rf(~addr), Bits(0))
    def write(addr: UInt, data: Bits) = {
      when (addr != UInt(0)) {
        rf(~addr) := data
      }
    }
  }

  val rf = new RegFile
  val id_addr = Vec(id_inst(19, 15), id_inst(24,20))
  val id_rs = id_addr.map(rf.read _)
  val id_rd = id_inst(11, 7)

  val alu = Module(new ALU)
  alu.io.in1 := id_rs(0)
  alu.io.in2 := id_rs(1)

  val waddr = id_rd
  val wdata = alu.io.out

  when (io.ctrl.wen) {
    rf.write(waddr, wdata)
  }

  io.ctrl.inst := id_inst
}

class Core(resetSignal: Bool = null) extends Module(_reset = resetSignal) with PCUParameters
{
  val io = new Bundle {
    val mem = new ScratchPadIO
  }

  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)
  val ibuf = Module(new InstLineBuffer)

  ibuf.io.cpu <> ctrl.io.imem
  ibuf.io.cpu <> dpath.io.imem
  ctrl.io.dpath <> dpath.io.ctrl

  io.mem <> ibuf.io.mem
}

class PCU extends Module with PCUParameters
{
  val io = new Bundle {
    val core_reset = Bool(INPUT)
    val spad = new MemPipeIO().flip
    val scr = new SCRIO
    val scr_busy = Bool(INPUT)
  }

  val core = Module(new Core(resetSignal = io.core_reset))

  val spad = Module(new ScratchPad)
  spad.io.cpu <> core.io.mem
  spad.io.mem <> io.spad
  
  io.scr.wen := Bool(false)
}
