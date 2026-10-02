import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._

/** Small test module: one 32-bit register at address 0x0 behind an [[ApbTarget]].
  * Every other address answers with an error. Counts read/write strobes so the
  * test can check they last exactly one cycle per transfer.
  */
class ApbTargetTestModule extends Module {
  val apb = IO(new ApbPort)
  val writes = IO(Output(UInt(8.W)))
  val reads = IO(Output(UInt(8.W)))
  val access = IO(Output(Bool()))

  val bus = new ApbTarget(apb)

  val reg = RegInit(0.U(32.W))
  val valid = bus.addr === 0.U
  when(bus.write && valid) { reg := bus.wdata }

  val wCount = RegInit(0.U(8.W))
  val rCount = RegInit(0.U(8.W))
  when(bus.write) { wCount := wCount + 1.U }
  when(bus.read) { rCount := rCount + 1.U }
  writes := wCount
  reads := rCount
  access := bus.access

  bus.respond(rdata = reg, error = !valid)
}

/** Part 2: tests for the APB handshake. */
class ApbTargetTest extends AnyFlatSpec with ChiselScalatestTester {

  def withBfm(body: (ApbTargetTestModule, ApbMasterBfm) => Unit): Unit =
    test(new ApbTargetTestModule) { dut =>
      val bfm = new ApbMasterBfm(
        dut.clock, dut.reset,
        dut.apb.psel, dut.apb.penable, dut.apb.paddr, dut.apb.pwrite,
        dut.apb.pwdata, dut.apb.prdata, dut.apb.pready, dut.apb.pslverr
      )
      bfm.reset()
      body(dut, bfm)
    }

  "ApbTarget" should "write and read back a register" in withBfm { (dut, bfm) =>
    assert(bfm.write(0x0, 0xcafebabeL).isDefined, "write to 0x0 should succeed")
    bfm.readExpect(0x0, Some(0xcafebabeL))
  }

  it should "signal an error for invalid addresses, for reads and writes" in withBfm { (dut, bfm) =>
    bfm.readExpect(0x4, None)
    assert(bfm.write(0x4, 0x1234).isEmpty, "write to 0x4 should fail")
    bfm.readExpect(0x0, Some(0)) // register untouched by the failed write
  }

  it should "produce exactly one read/write strobe per transfer" in withBfm { (dut, bfm) =>
    bfm.write(0x0, 1)
    bfm.write(0x0, 2)
    bfm.read(0x0)
    bfm.read(0x4) // strobes also fire for erroring transfers
    dut.writes.expect(2.U)
    dut.reads.expect(2.U)
  }

  it should "follow the setup/access phases with zero wait states" in withBfm { (dut, bfm) =>
    // idle: no access, no error, no read data
    dut.apb.psel.poke(false.B)
    dut.apb.penable.poke(false.B)
    dut.access.expect(false.B)
    dut.apb.pslverr.expect(false.B)
    dut.apb.prdata.expect(0.U)

    // setup phase of a read to an invalid address: not yet an access, no error yet
    dut.apb.psel.poke(true.B)
    dut.apb.penable.poke(false.B)
    dut.apb.pwrite.poke(false.B)
    dut.apb.paddr.poke(0x4.U)
    dut.access.expect(false.B)
    dut.apb.pslverr.expect(false.B)
    dut.clock.step()

    // access phase: ready immediately, error reported
    dut.apb.penable.poke(true.B)
    dut.access.expect(true.B)
    dut.apb.pready.expect(true.B)
    dut.apb.pslverr.expect(true.B)
    dut.clock.step()

    dut.apb.psel.poke(false.B)
    dut.apb.penable.poke(false.B)
    dut.access.expect(false.B)
    dut.apb.pslverr.expect(false.B)
  }
}
