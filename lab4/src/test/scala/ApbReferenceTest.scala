import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._

/** Our APB handshake plus the address check the Python adapter does: valid reads go to
  * readable registers, valid writes to writable ones, everything else is an error.
  * Test-only model (the real address decoding is part 3).
  */
class ApbAddressCheck(spec: SocSpec) extends Module {
  val apb = IO(new ApbPort)
  val bus = new ApbTarget(apb)

  private def hit(addrs: Seq[BigInt]): Bool =
    addrs.map(a => bus.addr === a.U(32.W)).foldLeft(false.B)(_ || _)

  bus.respond(
    rdata = 0.U,
    error = Mux(apb.pwrite, !hit(spec.writableAddresses), !hit(spec.readableAddresses))
  )
}

/** Drives our [[ApbAddressCheck]] and the Python reference adapter with the same bus
  * signals, so a test can compare their responses cycle by cycle.
  */
class ApbCompareHarness(spec: SocSpec) extends Module {
  val io = IO(new Bundle {
    val psel = Input(Bool())
    val penable = Input(Bool())
    val pwrite = Input(Bool())
    val paddr = Input(UInt(32.W))
    val pwdata = Input(UInt(32.W))
    val ourReady = Output(Bool())
    val ourError = Output(Bool())
    val refReady = Output(Bool())
    val refError = Output(Bool())
  })

  val ours = Module(new ApbAddressCheck(spec))
  ours.apb.psel := io.psel
  ours.apb.penable := io.penable
  ours.apb.pwrite := io.pwrite
  ours.apb.paddr := io.paddr
  ours.apb.pwdata := io.pwdata
  io.ourReady := ours.apb.pready
  io.ourError := ours.apb.pslverr

  val ref = Module(new PythonSocAdapter)
  ref.io.clock := clock
  ref.io.reset := reset.asBool
  ref.io.psel := io.psel
  ref.io.penable := io.penable
  ref.io.pwrite := io.pwrite
  ref.io.paddr := io.paddr
  ref.io.pwdata := io.pwdata
  // inputs from the IP blocks are irrelevant for the handshake
  ref.io.uart0_status_txEmpty := false.B
  ref.io.uart0_status_rxReady := false.B
  ref.io.uart0_data_rxData := 0.U
  ref.io.gpio0_dataIn := 0.U
  ref.io.gpio1_dataIn := 0.U
  io.refReady := ref.io.pready
  io.refError := ref.io.pslverr
}

/** Part 2, checked against the Python reference adapter.
  *
  * Runs hundreds of random APB transfers (valid and invalid addresses, reads and writes,
  * idle gaps and back-to-back transfers) on both adapters and checks that `pready` and
  * `pslverr` agree in every access-phase cycle. Needs Verilator: skipped locally
  * without it, a failure on CI.
  */
class ApbReferenceTest extends AnyFlatSpec with ChiselScalatestTester {

  val spec = SocSpec.load(PythonReference.SpecFile)

  def requireTools(): Unit = {
    val missing = PythonReference.load.left.toOption.orElse(
      if (PythonReference.verilatorAvailable) None
      else Some("Verilator is not installed (needed to simulate the Python reference adapter)")
    )
    missing.foreach(msg => if (PythonReference.onCi) fail(msg) else cancel(msg))
  }

  "ApbTarget" should "answer every transfer exactly like the Python reference adapter" in {
    requireTools()

    val validAddrs = (spec.readableAddresses ++ spec.writableAddresses).distinct
    val invalidAddrs = Seq[BigInt](
      0x0L, 0x41002ffcL, 0x41003001L, 0x41003002L, 0x4100300cL,
      0x41004020L, 0x7ffffffcL, 0x80000004L, 0xfffffffcL
    )
    val rnd = new scala.util.Random(42) // fixed seed: failures are reproducible

    test(new ApbCompareHarness(spec)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.psel.poke(false.B)
      dut.io.penable.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      for (i <- 0 until 500) {
        val addr =
          if (rnd.nextInt(5) == 0) invalidAddrs(rnd.nextInt(invalidAddrs.size))
          else validAddrs(rnd.nextInt(validAddrs.size))
        val write = rnd.nextBoolean()
        val what = s"transfer $i: ${if (write) "write" else "read"} 0x${addr.toString(16)}"

        // setup phase
        dut.io.psel.poke(true.B)
        dut.io.penable.poke(false.B)
        dut.io.pwrite.poke(write.B)
        dut.io.paddr.poke(addr.U)
        dut.io.pwdata.poke(BigInt(32, rnd).U)
        dut.clock.step()

        // access phase: both must be ready now and agree on the error response
        dut.io.penable.poke(true.B)
        val ourReady = dut.io.ourReady.peekBoolean()
        val refReady = dut.io.refReady.peekBoolean()
        assert(ourReady == refReady, s"$what: pready ours=$ourReady reference=$refReady")
        val ourError = dut.io.ourError.peekBoolean()
        val refError = dut.io.refError.peekBoolean()
        assert(ourError == refError, s"$what: pslverr ours=$ourError reference=$refError")
        dut.clock.step()

        // next: back-to-back transfer (psel stays high) or 1-2 idle cycles
        val gap = rnd.nextInt(3)
        if (gap > 0) {
          dut.io.psel.poke(false.B)
          dut.io.penable.poke(false.B)
          dut.clock.step(gap)
        }
      }
    }
  }
}
