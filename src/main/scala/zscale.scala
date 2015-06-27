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
    val bootmem = new HASTISlaveIO().flip
    val spi = new HASTISlaveIO().flip
    val led = new HASTISlaveIO().flip
    val dram = new HASTISlaveIO().flip
  }

  val core = Module(new Core(resetSignal = io.host.reset), {case TLId => "L1ToL2"})
  val bootmemafn = (addr: UInt) => addr(31, 14) === UInt(0)
  val spiafn = (addr: UInt) => addr(31, 14) === UInt(1)
  val ledafn = (addr: UInt) => addr(31, 14) === UInt(2)
  val dramafn = (addr: UInt) =>
    addr(31, 24) === UInt(1) ||
    addr(31, 24) === UInt(2) ||
    addr(31, 24) === UInt(3) ||
    addr(31, 24) === UInt(4)
  val xbar = Module(new HASTIXbar(2, Seq(bootmemafn, spiafn, ledafn, dramafn)))

  core.io.host <> io.host
  xbar.io.masters(0) <> core.io.dmem
  xbar.io.masters(1) <> core.io.imem
  io.bootmem <> xbar.io.slaves(0)
  io.spi <> xbar.io.slaves(1)
  io.led <> xbar.io.slaves(2)
  io.dram <> xbar.io.slaves(3)
}

class ZScaleTest extends Module with ZScaleParameters {
  val io = new Bundle {
    val host = new HTIFIO
  }

  val zscale = Module(new ZScale)
  val bootmem = Module(new HASTISRAM(4096))
  val dram = Module(new HASTISRAM(4194304))

  zscale.io.host <> io.host
  bootmem.io <> zscale.io.bootmem
  dram.io <> zscale.io.dram
}
