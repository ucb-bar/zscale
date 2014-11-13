// See LICENSE for license details.

package zscale

import Chisel._
import uncore._
import rocket._
import rocket.Util._

// TODO: merge with Rocket's CSRFile
class CSRFile extends Module with ZScaleParameters
{
  val io = new Bundle {
    val host = new HTIFIO
    val rw = new Bundle {
      val addr = UInt(INPUT, 12)
      val cmd = Bits(INPUT, CSR.SZ)
      val rdata = Bits(OUTPUT, xprLen)
      val wdata = Bits(INPUT, xprLen)
    }
    val scr = new SCRIO
    val masked_wen = Bool(INPUT) // to filter out csrrs/csrrc with x0

    val retire = Bool(INPUT)
    val xcpt = Bool(INPUT)
    val cause = UInt(INPUT, xprLen)
    val pc = UInt(INPUT, xprLen)
    val sret = Bool(INPUT)

    val status = new Status().asOutput
    val evec = UInt(OUTPUT, xprLen)
    val time = UInt(OUTPUT, 64)
  }

  val reg_status = Reg(new Status) // reset down below

  val reg_time = WideCounter(64)
  val reg_instret = WideCounter(64, io.retire)

  val reg_sup0 = Reg(Bits(width = xprLen))
  val reg_sup1 = Reg(Bits(width = xprLen))
  val reg_epc = Reg(UInt(width = xprLen))
  val reg_badvaddr = Reg(UInt(width = xprLen))
  val reg_compare = Reg(UInt(width = 32))
  val reg_evec = Reg(UInt(width = xprLen))
  val reg_cause = Reg(Bits(width = xprLen))
  val reg_tohost = Reg(init = Bits(0, xprLen))
  val reg_fromhost = Reg(init = Bits(0, xprLen))

  val r_irq_timer = Reg(init=Bool(false))
  val r_irq_ipi = Reg(init=Bool(true))

  val cpu_req_valid = io.rw.cmd != CSR.N
  val host_pcr_req_valid = Reg(Bool()) // don't reset
  val host_pcr_req_fire = host_pcr_req_valid && !cpu_req_valid
  val host_pcr_rep_valid = Reg(Bool()) // don't reset
  val host_pcr_bits = Reg(io.host.pcr_req.bits)
  io.host.pcr_req.ready := !host_pcr_req_valid && !host_pcr_rep_valid
  io.host.pcr_rep.valid := host_pcr_rep_valid
  io.host.pcr_rep.bits := host_pcr_bits.data
  when (io.host.pcr_req.fire()) {
    host_pcr_req_valid := true
    host_pcr_bits := io.host.pcr_req.bits
  }
  when (host_pcr_req_fire) {
    host_pcr_req_valid := false
    host_pcr_rep_valid := true
    host_pcr_bits.data := io.rw.rdata
  }
  when (io.host.pcr_rep.fire()) { host_pcr_rep_valid := false }

  // helper
  val addr = Mux(cpu_req_valid, io.rw.addr, host_pcr_bits.addr | 0x500)
  val decoded_addr = {
    val map = for ((v, i) <- csrList.zipWithIndex)
      yield v -> UInt(BigInt(1) << i)
    val out = ROM(map)(addr)
    Map((csrList zip out.toBools):_*)
  }

  io.host.ipi_req.valid := cpu_req_valid && decoded_addr(CSRs.send_ipi)
  io.host.ipi_req.bits := io.rw.wdata
  io.host.ipi_rep.ready := Bool(true)
  when (io.host.ipi_rep.valid) { r_irq_ipi := Bool(true) }

  val wen = cpu_req_valid || host_pcr_req_fire && host_pcr_bits.rw
  val wdata = Mux(cpu_req_valid, io.rw.wdata, host_pcr_bits.data)

  when (io.xcpt) {
    reg_status.s := Bool(true)
    reg_status.ps := reg_status.s
    reg_status.ei := Bool(false)
    reg_status.pei := reg_status.ei
    reg_epc := io.pc
    reg_badvaddr := io.rw.wdata
    reg_cause := io.cause
  }

  when (io.sret) {
    reg_status.s := reg_status.ps
    reg_status.ei := reg_status.pei
  }

  when (reg_time(reg_compare.getWidth-1, 0) === reg_compare) {
    r_irq_timer := Bool(true)
  }

  io.status := reg_status
  io.status.ef := false
  io.status.er := false
  io.status.u64 := false
  io.status.s64 := false
  io.status.vm := false
  io.status.zero := 0
  io.status.ip := Cat(r_irq_timer, reg_fromhost.orR, r_irq_ipi,   Bool(false),
                      Bool(false), Bool(false),      Bool(false), Bool(false))
  io.evec := Mux(io.xcpt, reg_evec, reg_epc)
  io.time := reg_time

  // read CSRs
  val read_mapping = collection.mutable.LinkedHashMap[Int,Bits](
    CSRs.cycle -> reg_time(31, 0),
    CSRs.cycleh -> reg_time(63, 32),
    CSRs.time -> reg_time(31, 0),
    CSRs.timeh -> reg_time(63, 32),
    CSRs.instret -> reg_instret(31, 0),
    CSRs.instreth -> reg_instret(63, 32),

    CSRs.sup0 -> reg_sup0,
    CSRs.sup1 -> reg_sup1,
    CSRs.epc -> reg_epc,
    CSRs.badvaddr -> reg_badvaddr,
    CSRs.count -> reg_time(31, 0),
    CSRs.compare -> reg_compare,
    CSRs.evec -> reg_evec,
    CSRs.cause -> reg_cause,
    CSRs.status -> io.status.toBits,
    CSRs.hartid -> io.host.id,
    CSRs.impl -> UInt(3), // ZScale
    CSRs.tohost -> reg_tohost,
    CSRs.fromhost -> reg_fromhost
  )

  // SCRs mapped into the CSR space
  for (i <- 0 until nSCR) {
    read_mapping += (CSRBaseForSCRs + i -> io.scr.rdata(i))
  }

  io.rw.rdata := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  // write CSRs
  val scr_wen = Bool()
  val scr_waddr = UInt()

  scr_wen := Bool(false)
  scr_waddr := UInt(0, log2Up(params(HTIFNSCR)))

  when (host_pcr_req_fire && !host_pcr_bits.rw && decoded_addr(CSRs.tohost)) { reg_tohost := UInt(0) }

  when (wen) {
    when (decoded_addr(CSRs.status)) { reg_status := new Status().fromBits(wdata) }
    when (decoded_addr(CSRs.sup0)) { reg_sup0 := wdata }
    when (decoded_addr(CSRs.sup1)) { reg_sup1 := wdata }
    when (decoded_addr(CSRs.epc)) { reg_epc := wdata }
    when (decoded_addr(CSRs.evec)) { reg_evec := wdata }
    when (decoded_addr(CSRs.count)) { reg_time := wdata }
    when (decoded_addr(CSRs.compare)) { reg_compare := wdata; r_irq_timer := Bool(false) }
    when (decoded_addr(CSRs.clear_ipi)) { r_irq_ipi := wdata(0) }
    when (decoded_addr(CSRs.tohost)) { when (reg_tohost === Bits(0) || host_pcr_req_fire) { reg_tohost := wdata } }
    when (decoded_addr(CSRs.fromhost)) { when (reg_fromhost === Bits(0) || !host_pcr_req_fire) { reg_fromhost := wdata } }

    // SCRs mapped into the CSR space
    when (io.rw.addr(11, 8) === UInt(4)) { scr_wen := io.masked_wen; scr_waddr := io.rw.addr }
  }

  io.scr.wen := scr_wen
  io.scr.waddr := scr_waddr
  io.scr.wdata := wdata

  when (this.reset) {
    reg_status.ei := false
    reg_status.pei := false
    reg_status.ps := false
    reg_status.s := true
    reg_status.im := 0
  }
}
