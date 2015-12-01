// See LICENSE for license details.

package zscale

import Chisel._
import cde.Parameters
import junctions._
import uncore._
import rocket._

trait HasZscaleParameters {
  implicit val p: Parameters
  val xLen = p(XLen)
  val coreInstBits = p(CoreInstBits)
  val fastMulDiv = p(FastMulDiv)

  // these should become parameters, rather than constants
  val haveMExt = true
  val haveEExt = false
}

abstract class ZscaleModule(implicit val p: Parameters) extends Module
  with HasZscaleParameters
abstract class ZscaleBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
  with HasZscaleParameters

class Zscale(resetSignal: Bool = null)(implicit val p: Parameters) extends Module(_reset = resetSignal)
    with HasZscaleParameters {
  val io = new Bundle {
    val imem = new HastiMasterIO
    val dmem = new HastiMasterIO
    val host = new HtifIO
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
