package zscale

import Chisel._
import cde.Parameters
import junctions._

class HastiSRAM(depth: Int)(implicit p: Parameters) extends HastiModule()(p) {
  val io = new HastiSlaveIO

  val wdata = Vec.tabulate(hastiDataBits/8)(i => io.hwdata(8*(i+1)-1,8*i))
  val waddr = Reg(UInt(width = hastiAddrBits))
  val wvalid = Reg(init = Bool(false))
  val wsize = Reg(UInt(width = SZ_HSIZE))
  val ram = SeqMem(depth, Vec(hastiDataBits/8, Bits(width = 8)))

  val wmask_lut = MuxLookup(wsize, Bits(0xf), Seq(
        UInt(0) -> Bits(0x1),
        UInt(1) -> Bits(0x3)))
  val wmask = (wmask_lut << waddr(1,0))(hastiDataBits / 8 - 1, 0)

  val is_trans = io.hsel && (io.htrans === HTRANS_NONSEQ || io.htrans === HTRANS_SEQ)
  val raddr = io.haddr >> UInt(2)
  val ren = is_trans && !io.hwrite
  val bypass = Reg(init = Bool(false))
  val last_wdata = Reg(next = wdata)
  val last_wmask = Reg(next = wmask)

  when (is_trans && io.hwrite) {
    waddr := io.haddr
    wsize := io.hsize
    wvalid := Bool(true)
  } .otherwise { wvalid := Bool(false) }

  when (ren) { bypass := wvalid && (waddr >> UInt(2)) === raddr }

  when (wvalid) {
    ram.write(waddr >> UInt(2), wdata, wmask.toBools)
  }

  val rdata = ram.read(raddr, ren)
  io.hrdata := Cat(rdata.zip(wmask.toBools).zip(wdata).map {
    case ((rbyte, wsel), wbyte) => Mux(wsel && bypass, wbyte, rbyte)
  }.reverse)

  io.hreadyout := Bool(true)
  io.hresp := HRESP_OKAY
}
