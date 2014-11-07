// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._
import rocket.Util._

class MemIOSplitter extends Module {
  val io = new Bundle {
    val hub = new MemPipeIO().flip
    val serdes = new MemPipeIO
    val spad = new MemPipeIO
  }

  val addr_msb = io.hub.req_cmd.bits.addr(params(MIFAddrBits)-1)
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

class ScratchPad extends Module {
  val io = new Bundle {
    val mem = new MemPipeIO().flip
  }

  val s_idle :: s_read :: s_write :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)
  val addr = Reg(UInt())
  val tag = Reg(UInt())
  val cnt = Reg(UInt(width = 2))

  when (state === s_idle && io.mem.req_cmd.valid) {
    state := Mux(io.mem.req_cmd.bits.rw, s_write, s_read)
    addr := io.mem.req_cmd.bits.addr
    tag := io.mem.req_cmd.bits.tag
    cnt := UInt(0)
  }
  when (state === s_read) {
    cnt := cnt + UInt(1)
    when (cnt === UInt(3)) {
      state := s_idle
    }
  }
  when (state === s_write && io.mem.req_data.valid) {
    cnt := cnt + UInt(1)
    when (cnt === UInt(3)) {
      state := s_idle
    }
  }

  io.mem.req_cmd.ready := (state === s_idle)
  io.mem.req_data.ready := (state === s_write)

  val ram = Mem(Bits(width = 128), 512, seqRead = true)
  val raddr = Reg(Bits())

  when (state === s_read) {
    raddr := Cat(addr, cnt)
  }
  when (io.mem.req_data.fire()) {
    ram(Cat(addr, cnt)) := io.mem.req_data.bits.data
  }

  io.mem.resp.valid := Reg(next = state === s_read)
  io.mem.resp.bits.data := ram(raddr)
  io.mem.resp.bits.tag := tag
}

class PCU extends Module {
  val io = new Bundle {
    val spad = new MemPipeIO().flip
  }

  val spad = Module(new ScratchPad)

  spad.io.mem <> io.spad
}
