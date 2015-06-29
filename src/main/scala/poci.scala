package zscale

import Chisel._

abstract trait POCIConstants
{
  val SZ_PADDR = 32
  val SZ_PDATA = 32
}

class POCIIO extends Bundle
{
  val paddr = UInt(OUTPUT, SZ_PADDR)
  val pwrite = Bool(OUTPUT)
  val psel = Bool(OUTPUT)
  val penable = Bool(OUTPUT)
  val pwdata = UInt(OUTPUT, SZ_PDATA)
  val prdata = UInt(INPUT, SZ_PDATA)
  val pready = Bool(INPUT)
  val pslverr = Bool(INPUT)
}

class HASTItoPOCIBridge extends Module
{
  val io = new Bundle {
    val in = new HASTISlaveIO
    val out = new POCIIO
  }

  io.out.paddr := io.in.haddr
  io.out.pwrite := io.in.hwrite
  io.out.psel := io.in.hsel && io.in.hreadyin
  io.out.penable := io.in.htrans.orR
  io.out.pwdata := io.in.hwdata
  io.in.hrdata := io.out.prdata
  io.in.hreadyout := io.out.pready
  io.in.hresp := io.out.pslverr
}

class POCIBus(amap: Seq[UInt=>Bool]) extends Module
{
  val io = new Bundle {
    val master = new POCIIO().flip
    val slaves = Vec.fill(amap.size){new POCIIO}
  }

  val s1_psels = Vec.fill(amap.size){Reg(Bool())}

  val psels = PriorityEncoderOH(
    (io.slaves zip amap) map { case (s, afn) => {
      s.paddr := io.master.paddr
      s.pwrite := io.master.pwrite
      s.penable := io.master.penable
      s.pwdata := io.master.pwdata
      afn(io.master.paddr)
  }})

  (io.slaves zip psels) foreach { case (s, psel) => {
    s.psel := psel
  } }

  (io.slaves zip s1_psels zip psels) foreach { case ((s, r_psel), psel) =>
    when (s.pready) { r_psel := psel }
  }

  io.master.prdata := Mux1H(s1_psels, io.slaves.map(_.prdata))
  io.master.pready := Mux1H(s1_psels, io.slaves.map(_.pready))
  io.master.pslverr := Mux1H(s1_psels, io.slaves.map(_.pslverr))
}
