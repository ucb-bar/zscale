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
  val arbFast = true

  val xprLen = 32
  val addrBits = log2Up(spadSize)
  val coreInstBits = params(CoreInstBits)
  val coreInstBytes = coreInstBits / 8

  val nSCR = params(HTIFNSCR)
  require(log2Up(nSCR) <= 8)

  val csrList = collection.mutable.ArrayBuffer[Int]()

  csrList += CSRs.cycle
  csrList += CSRs.cycleh
  csrList += CSRs.time
  csrList += CSRs.timeh
  csrList += CSRs.instret
  csrList += CSRs.instreth

  csrList += CSRs.sup0
  csrList += CSRs.sup1
  csrList += CSRs.epc
  csrList += CSRs.badvaddr
  csrList += CSRs.count
  csrList += CSRs.compare
  csrList += CSRs.evec
  csrList += CSRs.cause
  csrList += CSRs.status
  csrList += CSRs.hartid
  csrList += CSRs.impl
  csrList += CSRs.send_ipi
  csrList += CSRs.clear_ipi
  csrList += CSRs.tohost
  csrList += CSRs.fromhost

  val CSRBaseForSCRs = 0x400

  for (i <- 0 until nSCR) {
    csrList += (CSRBaseForSCRs + i)
  }
}

class Core(resetSignal: Bool = null) extends Module(_reset = resetSignal) with ZScaleParameters
{
  val io = new Bundle {
    val mem = new ScratchPadIO
    val host = new HTIFIO
    val scr = new SCRIO
    val scr_ready = Bool(INPUT)
  }

  val ctrl = Module(new Control)
  val dpath = Module(new Datapath)
  val ibuf = Module(new InstLineBuffer)
  val arb = Module(new MemArbiter)

  ibuf.io.cpu <> ctrl.io.imem
  ibuf.io.cpu <> dpath.io.imem
  ctrl.io.dpath <> dpath.io.ctrl

  arb.io.imem <> ibuf.io.mem
  arb.io.dmem <> ctrl.io.dmem
  arb.io.dmem <> dpath.io.dmem
  arb.io.dmem_fast_arb := ctrl.io.dmem_fast_arb
  io.mem <> arb.io.mem

  ctrl.io.host <> io.host
  dpath.io.host <> io.host

  ctrl.io.scr_ready := io.scr_ready
  io.scr <> ctrl.io.scr
  io.scr <> dpath.io.scr
}

// TODO: change scr write port into Decoupled
class ZScale extends Module with ZScaleParameters
{
  val io = new Bundle {
    val spad = new MemPipeIO().flip
    val host = new HTIFIO
    val scr = new SCRIO
    val scr_ready = Bool(INPUT)
  }

  val core = Module(new Core(resetSignal = io.host.reset))
  val spad = Module(new ScratchPad)

  spad.io.cpu <> core.io.mem
  spad.io.mem <> io.spad

  core.io.host <> io.host
  io.scr <> core.io.scr
  core.io.scr_ready := io.scr_ready
}
