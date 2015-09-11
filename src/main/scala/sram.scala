package zscale

import Chisel._
import junctions._

class HASTISRAM(depth: Int) extends Module with HASTIConstants
{
  val io = new HASTISlaveIO

  val wdata = Reg(Vec(Bits(width = 8), SZ_HDATA/8))
  val waddr = Reg(UInt(width = SZ_HADDR))
  val wvalid = Reg(init = Bool(false))
  val wsize = Reg(UInt(width = SZ_HSIZE))
  val ram = SeqMem(wdata, depth)

  val wmask_lut = MuxLookup(wsize, Bits(0xf), Seq(
        UInt(0) -> Bits(0x1),
        UInt(1) -> Bits(0x3)))
  val wmask = wmask_lut << waddr(1,0)

  val s_w1 :: s_w2 :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_w1)

  when (state === s_w2) {
    wdata := Vec.tabulate(SZ_HDATA/8)(i => io.hwdata(8*(i+1)-1,8*i))
    state := s_w1
  }

  val raddr = io.haddr >> UInt(2)
  val ren = Bool()
  val bypass = Reg(Bool())

  ren := Bool(false)

  when (io.hsel && (io.htrans === HTRANS_NONSEQ)) {
    when (io.hwrite) {
      waddr := io.haddr
      wsize := io.hsize
      wvalid := Bool(true)
      when (wvalid) {
        ram.write(waddr >> UInt(2), wdata, wmask.toBools)
      }
      state := s_w2
    } .otherwise {
      ren := Bool(true)
      bypass := ((waddr >> UInt(2)) === raddr) && wvalid
    }
  }

  val rdata = ram.read(raddr, ren).toBits
  val rmask = FillInterleaved(8, wmask & Fill(SZ_HDATA / 8, bypass))
  io.hrdata := (wdata.toBits & rmask) | (rdata & ~rmask)

  io.hreadyout := Bool(true)
  io.hresp := HRESP_OKAY
}
