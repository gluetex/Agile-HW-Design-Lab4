import java.io.File
import scala.io.Source
import scala.sys.process._
import scala.util.Try

/** The Python reference adapter (`soc_adapter.sv`), parsed into plain Scala values.
  *
  * The Python generator is the course's reference implementation, so it is an
  * independent "correct answer" to check our Chisel generator against.
  * Names are the flat port names (`uart0_ctrl_en`), addresses are absolute.
  */
case class PythonReference(
    ports: Seq[PythonReference.Port],
    readableAddresses: Seq[BigInt],
    writableAddresses: Seq[BigInt],
    writes: Set[PythonReference.Access], // fields software can write
    reads: Set[PythonReference.Access], // fields software reads from a register or block input
    consts: Set[(PythonReference.Access, BigInt)], // hardwired fields and their value
    resets: Map[String, (Int, BigInt)], // register name -> (width, reset value)
    triggers: Set[(String, String, BigInt)] // (field name, "wr" | "rd", address)
)

object PythonReference {

  case class Port(name: String, dir: String, width: Int)
  case class Access(name: String, hi: Int, lo: Int, address: BigInt)

  val SpecFile = "soc.xlsx"
  val GeneratorScript = "csr_adapter_gen.py"
  val OutputFile = "soc_adapter.sv"

  /** On CI (GitHub sets CI=true) a missing tool is a failure; locally the test is just skipped. */
  val onCi: Boolean = sys.env.get("CI").contains("true")

  /** Generated and parsed once per test run (thread-safe, shared by all suites). */
  lazy val load: Either[String, PythonReference] = generate().map(_ => parse(OutputFile))

  /** (Re)run the Python generator if its output is missing or older than its inputs. */
  def generate(): Either[String, Unit] = {
    val out = new File(OutputFile)
    val upToDate = out.exists &&
      out.lastModified >= new File(SpecFile).lastModified &&
      out.lastModified >= new File(GeneratorScript).lastModified
    if (upToDate) Right(())
    else {
      val pythons = Seq("python3", "python", "py")
      val ok = pythons.exists { py =>
        Try(Process(Seq(py, GeneratorScript, SpecFile)).!(ProcessLogger(_ => ()))).toOption.contains(0)
      }
      if (ok && out.exists) Right(())
      else Left(s"Could not run '$GeneratorScript'. Install Python and run: pip install -r requirements.txt")
    }
  }

  def verilatorAvailable: Boolean =
    Try(Process(Seq("verilator", "--version")).!(ProcessLogger(_ => ()))).toOption.contains(0)

  private def hex(s: String) = BigInt(s, 16)

  def parse(path: String): PythonReference = {
    val src = Source.fromFile(path)
    val lines = try src.getLines().toList finally src.close()
    val text = lines.mkString("\n")

    val bus = Set("clock", "reset", "psel", "penable", "paddr", "pwrite", "pwdata", "prdata", "pready", "pslverr")
    val PortRe = """^\s*(input|output) logic (?:\[(\d+):0\] )?(\w+),?\s*$""".r
    val ports = lines.collect {
      case PortRe(dir, msb, name) if !bus(name) => Port(name, dir, Option(msb).map(_.toInt + 1).getOrElse(1))
    }

    // the two "...: pslverr = 1'b0;" case lines list the readable, then the writable addresses
    val okLists = lines.filter(_.contains("pslverr = 1'b0")).map { l =>
      """32'h([0-9A-Fa-f]+)""".r.findAllMatchIn(l).map(m => hex(m.group(1))).toSeq
    }
    require(okLists.size == 2, s"Unexpected $path format: expected 2 pslverr address lists, found ${okLists.size}")

    // walk the case statements, remembering the address of the current case label
    val LabelRe = """^\s*32'h([0-9A-Fa-f]+):.*""".r
    val WriteRe = """(\w+)_reg <= pwdata\[(\d+):(\d+)\]""".r
    val ReadRe = """prdata\[(\d+):(\d+)\] = (\w+?)(?:_reg)?;""".r
    val ConstRe = """prdata\[(\d+):(\d+)\] = \d+'h([0-9A-Fa-f]+); // (\w+)""".r
    var addr = BigInt(-1)
    val writes = Set.newBuilder[Access]
    val reads = Set.newBuilder[Access]
    val consts = Set.newBuilder[(Access, BigInt)]
    lines.foreach { l =>
      l match {
        case LabelRe(a) => addr = hex(a)
        case _ =>
      }
      WriteRe.findAllMatchIn(l).foreach(m => writes += Access(m.group(1), m.group(2).toInt, m.group(3).toInt, addr))
      ReadRe.findAllMatchIn(l).foreach(m => reads += Access(m.group(3), m.group(1).toInt, m.group(2).toInt, addr))
      ConstRe.findAllMatchIn(l).foreach { m =>
        consts += (Access(m.group(4), m.group(1).toInt, m.group(2).toInt, addr) -> hex(m.group(3)))
      }
    }

    val resets = """(\w+)_reg <= (\d+)'h([0-9A-Fa-f]+);""".r.findAllMatchIn(text)
      .map(m => m.group(1) -> (m.group(2).toInt, hex(m.group(3)))).toMap

    val triggers = """assign (\w+)_trg = (wr|rd)_access && \(paddr == 32'h([0-9A-Fa-f]+)\)""".r
      .findAllMatchIn(text).map(m => (m.group(1), m.group(2), hex(m.group(3)))).toSet

    PythonReference(ports, okLists(0), okLists(1), writes.result(), reads.result(), consts.result(), resets, triggers)
  }
}
