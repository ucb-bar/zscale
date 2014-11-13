// See LICENSE for license details.

package zscale

import Chisel._
import uncore._

class MemIOSplitter extends Module with ZScaleParameters
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

class ScratchPadRequest extends Bundle with ZScaleParameters
{
  val addr = UInt(width = spadAddrBits)
  val rw = Bool()
  val wmask = Bits(width = spadByteMaskBits)
  val data = Bits(width = spadWidth)
  val tag = UInt(width = spadTagBits)
}

class ScratchPadResponse extends Bundle with ZScaleParameters
{
  val data = Bits(width = spadWidth)
  val tag = UInt(width = spadTagBits)
}

class ScratchPadIO extends Bundle with ZScaleParameters
{
  val req = Decoupled(new ScratchPadRequest)
  val resp = Valid(new ScratchPadResponse).flip
}

class ScratchPad extends Module with ZScaleParameters
{
  val io = new Bundle {
    val cpu = new ScratchPadIO().flip
    val mem = new MemPipeIO().flip
  }

  val s_idle :: s_read :: s_write :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  val ram = Mem(Bits(width = spadWidth), spadDepth, seqRead = true)
  val ren = Bool()
  val raddr = UInt()
  val rtag = Reg(UInt())
  val rdata = Bits()
  val wen = Bool()
  val waddr = UInt()
  val wdata = Bits()
  val wmask = Bits()

  // back side
  val addr = Reg(UInt())
  val tag = Reg(UInt())
  val cnt = Reg(UInt(width = log2Up(memDataBeats)))

  // ram read port
  ren := state === s_idle && io.cpu.req.valid && !io.cpu.req.bits.rw
  raddr := io.cpu.req.bits.addr
  rdata := ram(RegEnable(raddr, ren))

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
      rtag := io.cpu.req.bits.tag
    }
  }

  io.cpu.resp.valid := Reg(next = io.cpu.req.fire() && !io.cpu.req.bits.rw)
  io.cpu.resp.bits.data := rdata
  io.cpu.resp.bits.tag := rtag

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
    ren := Bool(true)
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
