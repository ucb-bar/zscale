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

  val spadSize = 32768
  require(isPow2(spadSize))
  val spadWidth = memDataBits
  val spadWordBytes = spadWidth / 8
  val spadDepth = spadSize / spadWordBytes
  val spadByteMaskBits = spadWordBytes
  val spadAddrBits = log2Up(spadDepth)
  val spadTagBits = 1

  val spadRespStages = 0
  val dmemRespStages = 0
  val arbFast = true

  val xprLen = 32
  val addrBits = log2Up(spadSize)
  val coreInstBits = params(CoreInstBits)
  val coreInstBytes = coreInstBits / 8

  val nSCR = params(HTIFNSCR)
  require(log2Up(nSCR) <= 8)
}

class Core(resetSignal: Bool = null) extends Module(_reset = resetSignal) with ZScaleParameters
{
  val io = new Bundle {
    val imem = new HASTIMasterIO
    val dmem = new HASTIMasterIO
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

class ZScale extends Module with ZScaleParameters {
  val io = new Bundle {
    val host = new HTIFIO
  }

  val core = Module(new Core(resetSignal = io.host.reset), {case TLId => "L1ToL2"})
  val imem = Module(new HASTISRAM(8192))
  val dmem = Module(new HASTISRAM(8192))

  core.io.host <> io.host

  imem.io <> core.io.imem
  core.io.imem.hready := imem.io.hreadyout
  imem.io.hsel := Bool(true)
  imem.io.hreadyin := imem.io.hreadyout

  dmem.io <> core.io.dmem
  core.io.dmem.hready := dmem.io.hreadyout
  dmem.io.hsel := Bool(true)
  dmem.io.hreadyin := dmem.io.hreadyout
}
