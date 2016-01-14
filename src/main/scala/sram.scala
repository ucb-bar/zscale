package zscale

import Chisel._
import cde.Parameters
import junctions._

class HastiSRAM(depth: Int)(implicit p: Parameters) extends HastiModule()(p) {
  val io = new HastiSlaveIO

  val wdata = Reg(Vec(hastiDataBits/8, Bits(width = 8)))
  val waddr = Reg(UInt(width = hastiAddrBits))
  val wvalid = Reg(init = Bool(false))
  val wsize = Reg(UInt(width = SZ_HSIZE))
  val ram = SeqMem(depth, Vec(hastiDataBits/8, Bits(width = 8)))

  val wmask_lut = MuxLookup(wsize, Bits(0xf), Seq(
        UInt(0) -> Bits(0x1),
        UInt(1) -> Bits(0x3)))
  val wmask = wmask_lut << waddr(1,0)

  val s_w1 :: s_w2 :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_w1)

  when (state === s_w2) {
    wdata := Vec.tabulate(hastiDataBits/8)(i => io.hwdata(8*(i+1)-1,8*i))
    state := s_w1
  }

  val raddr = io.haddr >> UInt(2)
  val ren = Wire(init=Bool(false))
  val bypass = Reg(Bool())

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
  val rmask = FillInterleaved(8, wmask & Fill(hastiDataBits / 8, bypass))
  io.hrdata := (wdata.toBits & rmask) | (rdata & ~rmask)

  io.hreadyout := Bool(true)
  io.hresp := HRESP_OKAY
}
