import chisel3._

/** APB target (slave) port, as seen from the CSR adapter. */
class ApbPort extends Bundle {
  val psel = Input(Bool())
  val penable = Input(Bool())
  val pwrite = Input(Bool())
  val paddr = Input(UInt(32.W))
  val pwdata = Input(UInt(32.W))
  val prdata = Output(UInt(32.W))
  val pready = Output(Bool())
  val pslverr = Output(Bool())
}

/** APB handshake logic for a target with zero wait states.
  *
  * An APB transfer has a setup phase (`psel` high, `penable` low) followed by
  * an access phase (`psel` and `penable` high). This target always answers in
  * the first access cycle: `pready` is high, so every transfer takes exactly
  * two cycles and the access phase lasts exactly one cycle. That makes
  * [[read]] and [[write]] single-cycle strobes, which can be used directly as
  * register write enables and as `trg` event pulses.
  *
  * Usage inside a module:
  * {{{
  * val apb = IO(new ApbPort)
  * val bus = new ApbTarget(apb)
  * when(bus.write && bus.addr === 0x1000.U) { myReg := bus.wdata }
  * bus.respond(rdata = readMux, error = !addressValid)
  * }}}
  */
class ApbTarget(port: ApbPort) {

  /** Setup phase: target selected, waiting for the access phase. */
  val setup: Bool = port.psel && !port.penable

  /** Access phase: the transfer completes in this cycle. */
  val access: Bool = port.psel && port.penable

  /** One-cycle strobe: a write transfer completes this cycle. */
  val write: Bool = access && port.pwrite

  /** One-cycle strobe: a read transfer completes this cycle. */
  val read: Bool = access && !port.pwrite

  /** Address of the current transfer (stable through setup and access). */
  val addr: UInt = port.paddr

  /** Write data of the current transfer. */
  val wdata: UInt = port.pwdata

  /** Drive the response signals. Call exactly once.
    *
    * @param rdata read data; only put on the bus during a read access, 0 otherwise
    * @param error the current address/direction is not allowed; turned into
    *              `pslverr` during the access phase
    */
  def respond(rdata: UInt, error: Bool): Unit = {
    port.pready := true.B
    port.prdata := Mux(read, rdata, 0.U)
    port.pslverr := access && error
  }
}
