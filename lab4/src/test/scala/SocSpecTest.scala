import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import help.Sheet

/** Part 1: tests for parsing and validating the CSR spreadsheet. No simulation needed. */
class SocSpecTest extends AnyFlatSpec with Matchers {

  behavior of "SocSpec.load(soc.xlsx)"

  val spec = SocSpec.load("soc.xlsx")

  it should "find all block instances of the memory map" in {
    spec.blocks.map(_.name) shouldBe Seq("uart0", "gpio0", "gpio1", "sysInfo")
    spec.blocks.map(_.blockType) shouldBe Seq("Uart", "Gpio", "Gpio", "SysInfo")
    spec.blocks.find(_.name == "gpio1").get.base shouldBe BigInt("41004010", 16)
  }

  it should "group rows into registers with fields" in {
    val uart = spec.blocks.find(_.name == "uart0").get
    uart.registers.map(_.name) shouldBe Seq("ctrl", "status", "data")
    uart.registers.map(_.offset) shouldBe Seq(BigInt(0), BigInt(4), BigInt(8))

    val data = uart.registers.find(_.name == "data").get
    data.fields.map(_.name) shouldBe Seq(Some("txData"), Some("rxData"))
    data.fields.map(_.typ) shouldBe Seq(FieldType.WoTrg, FieldType.RoTrg)
    data.swReadable shouldBe true
    data.swWritable shouldBe true
  }

  it should "parse ranges, widths and init values" in {
    val ctrl = spec.blocks.head.registers.head
    ctrl.fields.map(f => (f.hi, f.lo, f.width)) shouldBe Seq((0, 0, 1), (1, 1, 1))
    ctrl.fields.map(_.init) shouldBe Seq(Some(BigInt(0)), Some(BigInt(0))) // numeric 0 cells

    val id = spec.blocks.find(_.name == "sysInfo").get.registers.head.fields.head
    id.name shouldBe None
    id.typ shouldBe FieldType.Const
    id.init shouldBe Some(BigInt("deadbeef", 16))

    val dataIn = spec.blocks.find(_.name == "gpio0").get.registers.find(_.name == "dataIn").get
    dataIn.fields.head.init shouldBe None // '?'
  }

  it should "compute absolute addresses and access sets" in {
    spec.readableAddresses should contain(BigInt("41004008", 16)) // gpio0.dataIn
    spec.readableAddresses should contain(BigInt("80000000", 16)) // sysInfo.id
    spec.writableAddresses should not contain (BigInt("41003004", 16)) // uart0.status (ro)
    spec.writableAddresses should contain(BigInt("41003008", 16)) // uart0.data (wotrg)
  }

  it should "name the IP-side ports like the Python generator" in {
    spec.ports.map(_._1) shouldBe Seq(
      "uart0_ctrl_en", "uart0_ctrl_loopback",
      "uart0_status_txEmpty", "uart0_status_rxReady",
      "uart0_data_txData", "uart0_data_txData_trg",
      "uart0_data_rxData", "uart0_data_rxData_trg",
      "gpio0_ctrl_en", "gpio0_dir", "gpio0_dataIn", "gpio0_dataOut",
      "gpio1_ctrl_en", "gpio1_dir", "gpio1_dataIn", "gpio1_dataOut"
    )
  }

  behavior of "SocSpec parsing helpers"

  it should "parse numbers in the forms the spreadsheet produces" in {
    SocSpec.parseNumber("0x1F") shouldBe Some(BigInt(31))
    SocSpec.parseNumber("0.0") shouldBe Some(BigInt(0))
    SocSpec.parseNumber("7") shouldBe Some(BigInt(7))
    SocSpec.parseNumber("?") shouldBe None
    SocSpec.parseNumber("") shouldBe None
  }

  it should "parse bit ranges" in {
    SocSpec.parseRange("31:12", "x") shouldBe ((31, 12))
    SocSpec.parseRange("3", "x") shouldBe ((3, 3))
    an[IllegalArgumentException] should be thrownBy SocSpec.parseRange("a:b", "x")
  }

  behavior of "SocSpec validation"

  val mapHeader = Seq("Block", "Name", "Interface", "Base Address", "End Address")
  val regHeader = Seq("Register", "Offset", "Field", "Type", "Range", "Init")

  def specWith(regRows: Seq[Seq[String]], mapRows: Seq[Seq[String]] = Seq(Seq("B", "b0", "APB", "0x1000", "0x10FF"))) =
    SocSpec.fromSheets(Map("Map" -> new Sheet(mapHeader, mapRows), "B" -> new Sheet(regHeader, regRows)))

  def rejects(regRows: Seq[Seq[String]], mapRows: Seq[Seq[String]] = Seq(Seq("B", "b0", "APB", "0x1000", "0x10FF"))) =
    an[IllegalArgumentException] should be thrownBy specWith(regRows, mapRows)

  it should "accept the README example" in {
    val s = specWith(Seq(
      Seq("myReg0", "0x0", "foo", "rw", "11:0", "0x123"),
      Seq("myReg0", "0x0", "bar", "ro", "31:12", "?"),
      Seq("myReg1", "0x4", "", "rw", "7:0", "0x7"),
      Seq("myWrTrg", "0x8", "baz", "wotrg", "7:0", "?"),
      Seq("myRdTrg", "0xC", "qux", "rotrg", "15:0", "?"),
      Seq("myConst", "0x10", "", "const", "31:0", "0xcafebabe")
    ))
    s.fields.size shouldBe 6
  }

  it should "reject an unknown type" in rejects(Seq(Seq("r", "0x0", "", "rx", "7:0", "?")))
  it should "reject a range outside 31:0" in rejects(Seq(Seq("r", "0x0", "", "rw", "32:0", "0")))
  it should "reject an init value that does not fit" in rejects(Seq(Seq("r", "0x0", "", "rw", "3:0", "0x10")))
  it should "reject a const without init" in rejects(Seq(Seq("r", "0x0", "", "const", "31:0", "?")))
  it should "reject an unaligned offset" in rejects(Seq(Seq("r", "0x2", "", "rw", "7:0", "0")))
  it should "reject a register outside its block" in rejects(Seq(Seq("r", "0x100", "", "rw", "7:0", "0")))
  it should "reject overlapping readable fields" in rejects(Seq(
    Seq("r", "0x0", "a", "rw", "7:0", "0"),
    Seq("r", "0x0", "b", "ro", "3:0", "?")
  ))
  it should "reject overlapping blocks" in rejects(
    Seq(Seq("r", "0x0", "", "rw", "7:0", "0")),
    Seq(Seq("B", "b0", "APB", "0x1000", "0x10FF"), Seq("B", "b1", "APB", "0x10F0", "0x11FF"))
  )
  it should "reject a missing block sheet" in rejects(
    Seq(Seq("r", "0x0", "", "rw", "7:0", "0")),
    Seq(Seq("Nope", "b0", "APB", "0x1000", "0x10FF"))
  )
}
