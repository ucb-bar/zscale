// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._

abstract trait ZScaleParameters extends UsesParameters
{
  val memAddrBits = params(MIFAddrBits)
  val memDataBits = params(MIFDataBits)
  val memDataBeats = params(MIFDataBeats)

  val spadSize = 8192
  require(isPow2(spadSize))
  val spadWidth = memDataBits
  val spadWordBytes = spadWidth / 8
  val spadDepth = spadSize / spadWordBytes
  val spadByteMaskBits = spadWordBytes
  val spadAddrBits = log2Up(spadDepth)
  val spadTagBits = 1

  val spadRespStages = 0
  val dmemRespStages = 1
  val arbFast = true

  val xprLen = 32
  val addrBits = log2Up(spadSize)
  val coreInstBits = params(CoreInstBits)
  val coreInstBytes = coreInstBits / 8

  val nSCR = params(HTIFNSCR)
  require(log2Up(nSCR) <= 8)
}

class SRAMRequest extends Bundle with ZScaleParameters
{
  val rw = Bool()
  val addr = UInt(width = 13)
  val wmask = Bits(width = xprLen)
  val data = Bits(width = xprLen)
}

class SRAMResponse extends Bundle with ZScaleParameters
{
  val data = Bits(width = xprLen)
}

class SRAMIO extends Bundle with ZScaleParameters
{
  val req = Valid(new SRAMRequest)
  val resp = Valid(new SRAMResponse).flip
}

class SRAMInstIO extends SRAMIO
{
  val invalidate = Decoupled(Bits(width = 1))
}

class SRAMDataIO extends Bundle with ZScaleParameters
{
  val req = Decoupled(new SRAMRequest)
  val resp = Valid(new SRAMResponse).flip
}

class Core(resetSignal: Bool = null) extends Module(_reset = resetSignal) with ZScaleParameters
{
  val io = new Bundle {
    val imem = new SRAMInstIO
    val dmem = new SRAMDataIO
    val host = new HTIFIO
  }

  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)

  io.imem <> ctrl.io.imem
  io.imem <> dpath.io.imem
  io.dmem <> ctrl.io.dmem
  io.dmem <> dpath.io.dmem
  ctrl.io.dpath <> dpath.io.ctrl

  ctrl.io.host <> io.host
  dpath.io.host <> io.host
}

class ZScale extends Module {
  val io = new Bundle {
    val imem = new SRAMIO
    val dmem = new SRAMIO
    val host = new HTIFIO
  }

  val core = Module(new Core(resetSignal = io.host.reset), {case TLId => "L1ToL2"})

  core.io.host <> io.host
  io.imem <> core.io.imem
  io.dmem <> core.io.dmem

  core.io.imem.invalidate.ready := Bool(true)
  core.io.dmem.req.ready := Bool(true)
}
