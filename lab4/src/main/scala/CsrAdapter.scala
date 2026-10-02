
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

  // IP-side ports, one entry per field as listed in the README (e.g. uart0_ctrl_en,
  // uart0_data_txData + uart0_data_txData_trg)
  val csr = IO(new DynamicBundle(spec.ports))

  // TODO part 3: address decoding -> which register is accessed, error on invalid address
  // TODO part 4: field logic (rw/ro/const, wotrg/rotrg) -> drive csr outputs and read data
  csr.elements.values.foreach { port => // placeholder: leave outputs undriven for now
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
