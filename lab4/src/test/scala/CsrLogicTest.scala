import chisel3._
import chiseltest._
import help._
import org.scalatest.flatspec.AnyFlatSpec

class CsrLogicTest extends AnyFlatSpec with ChiselScalatestTester {

  /** Look up a nested csr port by path, e.g. port(dut, "uart0", "ctrl", "en") */
  def port(dut: CsrAdapter, path: String*): UInt =
    CsrPort.lookup(dut.csr, path).asInstanceOf[UInt]

  def newBfm(dut: CsrAdapter) = new ApbMasterBfm(
    dut.clock, dut.reset, dut.apb.psel, dut.apb.penable, dut.apb.paddr,
    dut.apb.pwrite, dut.apb.pwdata, dut.apb.prdata, dut.apb.pready, dut.apb.pslverr
  )

  "Step1" should "read the constant and reject unknown addresses" in {
    test(new CsrAdapter("soc.xlsx")) { dut =>
      val bfm = newBfm(dut)
      bfm.reset()
      bfm.readExpect(0x80000000L, Some(0xdeadbeefL)) // const
      bfm.readExpect(0x50000000L, None)              // address that does not exist
      bfm.readExpect(0x41003010L, None)              // in the gap between uart0 and gpio0
      assert(bfm.write(0x80000000L, 1).isEmpty)      // const cannot be written -> error
    }
  }

  "Step2" should "support rw and ro fields" in {
    test(new CsrAdapter("soc.xlsx")) { dut =>
      val bfm = newBfm(dut)
      bfm.reset()

      // reset values
      bfm.readExpect(0x41003000L, Some(0))
      port(dut, "gpio0", "dir").expect(0.U)

      // rw register with two 1-bit fields: only the lower 2 bits are writable
      bfm.write(0x41003000L, 0xdeadbeefL)
      bfm.readExpect(0x41003000L, Some(0x3))
      port(dut, "uart0", "ctrl", "en").expect(1.B)
      port(dut, "uart0", "ctrl", "loopback").expect(1.B)

      // writing 0 to only bit 0 clears en but keeps loopback
      bfm.write(0x41003000L, 0x2)
      port(dut, "uart0", "ctrl", "en").expect(0.B)
      port(dut, "uart0", "ctrl", "loopback").expect(1.B)

      // full 32 bit rw register, and the two gpio banks are independent
      bfm.write(0x41004004L, 0x12345678L)
      port(dut, "gpio0", "dir").expect(0x12345678L.U)
      port(dut, "gpio1", "dir").expect(0.U)
      bfm.readExpect(0x41004004L, Some(0x12345678L))
      bfm.readExpect(0x41004014L, Some(0))

      // ro register: value comes from the block
      port(dut, "gpio0", "dataIn").poke(0xcafef00dL.U)
      bfm.readExpect(0x41004008L, Some(0xcafef00dL))

      // ro register in two fields
      port(dut, "uart0", "status", "txEmpty").poke(1.B)
      port(dut, "uart0", "status", "rxReady").poke(0.B)
      bfm.readExpect(0x41003004L, Some(0x1))
      port(dut, "uart0", "status", "rxReady").poke(1.B)
      bfm.readExpect(0x41003004L, Some(0x3))

      // ro cannot be written
      assert(bfm.write(0x41004008L, 1).isEmpty)
      assert(bfm.write(0x41003004L, 1).isEmpty)
      // rw can
      assert(bfm.write(0x41004004L, 1).isDefined)
    }
  }

  "Step3" should "pulse the triggers of wotrg and rotrg fields" in {
    test(new CsrAdapter("soc.xlsx")) { dut =>
      val bfm = newBfm(dut)
      bfm.reset()

      // write trigger: txData gets the data and a one-cycle trg pulse in the access phase
      port(dut, "uart0", "data", "txData", "trg").expect(0.B)
      fork {
        bfm.write(0x41003008L, 0x5aL)
        port(dut, "uart0", "data", "txData", "data").expect(0x5a.U)
      }.fork
        .withRegion(Monitor) {
          dut.clock.step(1) // wait for access phase
          port(dut, "uart0", "data", "txData", "trg").expect(1.B)
          port(dut, "uart0", "data", "rxData", "trg").expect(0.B) // a write must not fire the read trigger
        }.joinAndStep()
      port(dut, "uart0", "data", "txData", "trg").expect(0.B)

      // read trigger: rxData comes from the block, and a one-cycle trg pulse in the access phase
      fork {
        port(dut, "uart0", "data", "rxData", "data").poke(0xaa.U)
        bfm.readExpect(0x41003008L, Some(0xaa))
      }.fork
        .withRegion(Monitor) {
          dut.clock.step(1) // wait for access phase
          port(dut, "uart0", "data", "rxData", "trg").expect(1.B)
          port(dut, "uart0", "data", "txData", "trg").expect(0.B) // a read must not fire the write trigger
        }.joinAndStep()
      port(dut, "uart0", "data", "rxData", "trg").expect(0.B)

      // no trigger without access: reading another register must not pulse anything
      bfm.readExpect(0x41003000L, Some(0))
      port(dut, "uart0", "data", "rxData", "trg").expect(0.B)
    }
  }
}