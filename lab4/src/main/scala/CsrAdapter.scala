
import help._

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror

class CsrAdapter(descriptionSheetPath: String) extends Module {

  // Part 1: parsed and validated CSR specification
  val spec = SocSpec.load(descriptionSheetPath)
  println(spec)

  // Part 2: APB interface and handshake
  val apb = IO(new ApbPort)
  val bus = new ApbTarget(apb)

  // IP-side ports, nested as in the README: csr.uart0.ctrl.en (rw),
  // csr.uart0.data.txData.data / .trg (wotrg). Look one up with CsrPort.lookup(csr, field.path)
  val csr = IO(spec.csrBundle())

  // ---- Part 3: address decoding ----
  def addrIs(a: BigInt): Bool = bus.addr === a.U(32.W)
  def addrIsOneOf(addrs: Seq[BigInt]): Bool = addrs.map(addrIs).foldLeft(false.B)(_ || _)

  val readable = addrIsOneOf(spec.readableAddresses)
  val writable = addrIsOneOf(spec.writableAddresses)
  val error = Mux(apb.pwrite, !writable, !readable)

  // ---- Part 4: field logic ----
  /** Register holding a software-writable field. Reset to Init (0 if the sheet has `?`). */
  def storageReg(f: CsrField): UInt = {
    val fld = f.field
    val reg = RegInit(fld.init.getOrElse(BigInt(0)).U(fld.width.W))
    when(bus.write && addrIs(f.address)) {
      reg := bus.wdata(fld.hi, fld.lo)
    }
    reg
  }

  // for every readable field: its value, moved to its bit position, and only if its register is addressed
  val readTerms = spec.fields.flatMap { f =>
    val fld = f.field
    val sel = addrIs(f.address)
    val value: Option[UInt] = fld.typ match {
      case FieldType.Const => Some(fld.init.get.U(fld.width.W))
      case FieldType.RW =>
        val reg = storageReg(f)
        CsrPort.lookup(csr, f.path) := reg
        Some(reg)
      case FieldType.RO =>
        Some(CsrPort.lookup(csr, f.path).asInstanceOf[UInt])
      case FieldType.WoTrg =>
        CsrPort.lookup(csr, f.path :+ "data") := storageReg(f)
        CsrPort.lookup(csr, f.path :+ "trg") := bus.write && sel
        None // write-only: nothing to read back
      case FieldType.RoTrg =>
        CsrPort.lookup(csr, f.path :+ "trg") := bus.read && sel
        Some(CsrPort.lookup(csr, f.path :+ "data").asInstanceOf[UInt])
    }
    value.map(v => Mux(sel, v << fld.lo, 0.U(32.W)))
  }
  val rdata = readTerms.foldLeft(0.U(32.W))(_ | _)

  bus.respond(rdata = rdata, error = error)

}

object CsrAdapter extends App {
  emitVerilog(
    new CsrAdapter("soc.xlsx"),
    Array("--target-dir", "generated")
  )
}
