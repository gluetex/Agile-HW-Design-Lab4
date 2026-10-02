import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import chisel3.reflect.DataMirror

/** Part 1, checked against the course's Python reference adapter instead of our own assumptions.
  *
  * Generates `soc_adapter.sv` with `csr_adapter_gen.py` and checks that our parsed
  * [[SocSpec]] describes exactly the same adapter: ports, address map, field ranges,
  * reset values, constants and triggers. Skipped locally when Python is missing,
  * a failure on CI.
  */
class ReferenceSpecTest extends AnyFlatSpec with Matchers {

  val spec = SocSpec.load(PythonReference.SpecFile)

  def reference: PythonReference = PythonReference.load match {
    case Right(r) => r
    case Left(msg) => if (PythonReference.onCi) fail(msg) else cancel(msg)
  }

  behavior of "SocSpec compared to the Python reference adapter"

  it should "have the same IP-side ports (name, direction, width, order)" in {
    val ours = spec.ports.map { case (name, data) =>
      val dir = DataMirror.specifiedDirectionOf(data) match {
        case SpecifiedDirection.Output => "output"
        case SpecifiedDirection.Input  => "input"
        case other => s"unexpected($other)"
      }
      PythonReference.Port(name, dir, data.getWidth)
    }
    ours shouldBe reference.ports
  }

  it should "have the same readable and writable addresses" in {
    spec.readableAddresses shouldBe reference.readableAddresses.sorted
    spec.writableAddresses shouldBe reference.writableAddresses.sorted
  }

  def access(f: CsrField) = PythonReference.Access(f.ioName, f.field.hi, f.field.lo, f.address)

  it should "place every software-writable field at the same address and bits" in {
    spec.fields.filter(_.field.typ.swWritable).map(access).toSet shouldBe reference.writes
  }

  it should "place every software-readable field at the same address and bits" in {
    val readable = spec.fields.filter(f => f.field.typ.swReadable && f.field.typ != FieldType.Const)
    readable.map(access).toSet shouldBe reference.reads
  }

  it should "have the same constants" in {
    val consts = spec.fields.filter(_.field.typ == FieldType.Const).map(f => access(f) -> f.field.init.get)
    consts.toSet shouldBe reference.consts
  }

  it should "have the same reset values" in {
    val resets = spec.fields
      .filter(f => f.field.typ.hasStorage && f.field.init.isDefined)
      .map(f => f.ioName -> (f.field.width, f.field.init.get))
      .toMap
    resets shouldBe reference.resets
  }

  it should "have the same read and write triggers" in {
    val triggers = spec.fields.collect {
      case f if f.field.typ == FieldType.WoTrg => (f.ioName, "wr", f.address)
      case f if f.field.typ == FieldType.RoTrg => (f.ioName, "rd", f.address)
    }
    triggers.toSet shouldBe reference.triggers
  }
}
