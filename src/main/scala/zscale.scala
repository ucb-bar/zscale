// See LICENSE for license details.

package zscale

import Chisel._
import junctions._
import uncore._
import rocket._

abstract trait ZscaleParameters extends UsesParameters
{
  val xLen = 32
  val coreInstBits = params(CoreInstBits)

  // these should become parameters, rather than constants
  val haveMExt = true
  val haveEExt = false
}

class Zscale(resetSignal: Bool = null) extends Module(_reset = resetSignal) with ZscaleParameters
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
