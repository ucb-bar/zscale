// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._

abstract trait ZScaleParameters extends UsesParameters
{
  val xprLen = 32
  val coreInstBits = params(CoreInstBits)
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
