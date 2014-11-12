// See LICENSE for license details.

package zscale

import Chisel._

class InstMemReq extends Bundle with ZScaleParameters
{
  val addr = UInt(width = addrBits)
}

class InstMemResp extends Bundle with ZScaleParameters
{
  val inst = Bits(width = coreInstBits)
}

class InstMemIO extends Bundle with ZScaleParameters
{
  val req = Valid(new InstMemReq)
  val resp = Valid(new InstMemResp).flip
  val invalidate = Decoupled(Bool())
}

class InstLineBuffer extends Module with ZScaleParameters
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
  io.cpu.invalidate.ready := (state === s_idle)

  when (io.cpu.invalidate.fire()) {
    tag_valid := Bool(false)
  }

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

class MemArbiter extends Module with ZScaleParameters
{
  val io = new Bundle {
    val imem = new ScratchPadIO().flip
    val dmem = new ScratchPadIO().flip
    val mem = new ScratchPadIO
  }

  io.imem.req.ready := io.mem.req.ready && !io.dmem.req.valid
  io.dmem.req.ready := io.mem.req.ready
  io.mem.req.valid := io.imem.req.valid || io.dmem.req.valid
  io.mem.req.bits := Mux(io.dmem.req.valid, io.dmem.req.bits, io.imem.req.bits)
  io.mem.req.bits.tag := io.dmem.req.valid

  io.imem.resp.valid := io.mem.resp.valid && (io.mem.resp.bits.tag === Bits(0))
  io.dmem.resp.valid := io.mem.resp.valid && (io.mem.resp.bits.tag === Bits(1))
  io.imem.resp.bits := io.mem.resp.bits
  io.dmem.resp.bits := io.mem.resp.bits
}
