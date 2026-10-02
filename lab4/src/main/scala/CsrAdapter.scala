
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

  // TODO part 3: address decoding -> which register is accessed, error on invalid address
  // TODO part 4: field logic (rw/ro/const, wotrg/rotrg) -> drive csr outputs and read data
  def leaves(d: Data): Seq[Data] = d match {
    case r: Record => r.elements.values.toSeq.flatMap(leaves)
    case leaf => Seq(leaf)
  }
  leaves(csr).foreach { port => // placeholder: leave outputs undriven for now
    if (DataMirror.specifiedDirectionOf(port) == SpecifiedDirection.Output) port := DontCare
  }
  bus.respond(rdata = 0.U, error = true.B)

}

object CsrAdapter extends App {
  emitVerilog(
    new CsrAdapter("soc.xlsx"),
    Array("--target-dir", "generated")
  )
}
