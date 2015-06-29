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
    val jtag = new HASTIMasterIO().flip
    val bootmem = new HASTISlaveIO().flip
    val dram = new HASTISlaveIO().flip
    val spi = new HASTISlaveIO().flip
    val led = new POCIIO
    val corereset = new POCIIO
  }

  val core = Module(new Core(resetSignal = io.host.reset), {case TLId => "L1ToL2"})

  val bootmem_afn = (addr: UInt) => addr(31, 14) === UInt(0)

  val sbus_afn = (addr: UInt) => addr(31, 29).orR
  val dram_afn = (addr: UInt) => addr(31, 26) === UInt(4)
  val spi_afn = (addr: UInt) => addr(31, 26) === UInt(5) && addr(25, 14) === UInt(0)

  val pbus_afn = (addr: UInt) => addr(31) === UInt(1)
  val led_afn = (addr: UInt) => addr(31) === UInt(1) && addr(30, 10) === UInt(0)
  val corereset_afn = (addr: UInt) => addr(31) === UInt(1) && addr(30, 10) === UInt(1)

  val xbar = Module(new HASTIXbar(3, Seq(bootmem_afn, sbus_afn)))
  val sadapter = Module(new HASTISlaveToMaster)
  val sbus = Module(new HASTIBus(Seq(dram_afn, spi_afn, pbus_afn)))
  val padapter = Module(new HASTItoPOCIBridge)
  val pbus = Module(new POCIBus(Seq(led_afn, corereset_afn)))

  core.io.host <> io.host
  xbar.io.masters(0) <> io.jtag
  xbar.io.masters(1) <> core.io.dmem
  xbar.io.masters(2) <> core.io.imem

  io.bootmem <> xbar.io.slaves(0)
  sadapter.io.in <> xbar.io.slaves(1)

  sbus.io.master <> sadapter.io.out
  io.dram <> sbus.io.slaves(0)
  io.spi <> sbus.io.slaves(1)
  padapter.io.in <> sbus.io.slaves(2)

  pbus.io.master <> padapter.io.out
  io.led <> pbus.io.slaves(0)
  io.corereset <> pbus.io.slaves(1)
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
