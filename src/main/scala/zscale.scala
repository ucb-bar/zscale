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
  val ibus = Module(new HASTIBus(Seq(_=>Bool(true))))
  val dbus = Module(new HASTIBus(Seq(_=>Bool(true))))
  val mux = Module(new HASTISlaveMux(2))
  val mem = Module(new HASTISRAM(8192))

  core.io.host <> io.host

  ibus.io.master <> core.io.imem
  dbus.io.master <> core.io.dmem
  mux.io.ins(0) <> dbus.io.slaves(0)
  mux.io.ins(1) <> ibus.io.slaves(0)
  mem.io <> mux.io.out
}
