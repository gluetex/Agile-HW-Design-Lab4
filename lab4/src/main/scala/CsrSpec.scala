import help._

import chisel3._

/** Data model of the CSR specification in the spreadsheet.
  *
  * `SocSpec.load("soc.xlsx")` turns the Excel description into plain Scala
  * values: one [[BlockInstance]] per row of the `Map` sheet, each holding the
  * [[Register]]s and [[Field]]s of its block type. Everything is validated on
  * load, so the hardware generator can trust the data it gets.
  */

/** Access type of a register or field (column `Type`). */
sealed abstract class FieldType(val name: String) {

  /** Software can read it over the bus. */
  def swReadable: Boolean

  /** Software can write it over the bus. */
  def swWritable: Boolean

  /** The adapter holds a register for it (written by software). */
  def hasStorage: Boolean = swWritable

  /** The adapter emits a one-cycle `trg` event to the block on access. */
  def hasTrigger: Boolean
}

object FieldType {

  /** software read/write, hardware reads (output to block) */
  case object RW extends FieldType("rw") {
    val swReadable = true; val swWritable = true; val hasTrigger = false
  }

  /** software read, hardware provides the value (input from block) */
  case object RO extends FieldType("ro") {
    val swReadable = true; val swWritable = false; val hasTrigger = false
  }

  /** software write only, plus a write event to the block */
  case object WoTrg extends FieldType("wotrg") {
    val swReadable = false; val swWritable = true; val hasTrigger = true
  }

  /** software read only, plus a read event; the block provides the value */
  case object RoTrg extends FieldType("rotrg") {
    val swReadable = true; val swWritable = false; val hasTrigger = true
  }

  /** software read only, value hardwired in the adapter */
  case object Const extends FieldType("const") {
    val swReadable = true; val swWritable = false; val hasTrigger = false
  }

  val all: Seq[FieldType] = Seq(RW, RO, WoTrg, RoTrg, Const)

  def parse(s: String): FieldType =
    all.find(_.name == s.trim.toLowerCase).getOrElse(
      throw new IllegalArgumentException(
        s"Unknown CSR type '$s' (expected one of ${all.map(_.name).mkString(", ")})"
      )
    )
}

/** A field of a register, occupying bits `hi:lo` of the 32-bit data word.
  *
  * @param name None when the register has no subfields (whole-register field)
  * @param init reset value (`rw`, `wotrg`) or constant value (`const`); None for `?`
  */
case class Field(
    name: Option[String],
    typ: FieldType,
    hi: Int,
    lo: Int,
    init: Option[BigInt]
) {
  def width: Int = hi - lo + 1

  /** Bit mask of this field inside the 32-bit word. */
  def mask: BigInt = ((BigInt(1) << width) - 1) << lo

  def rangeString: String = s"$hi:$lo"
}

/** A register at `offset` from the base of its block. */
case class Register(name: String, offset: BigInt, fields: Seq[Field]) {
  def swReadable: Boolean = fields.exists(_.typ.swReadable)
  def swWritable: Boolean = fields.exists(_.typ.swWritable)
}

/** One instance of an IP block in the memory map (a row of the `Map` sheet). */
case class BlockInstance(
    name: String,
    blockType: String,
    base: BigInt,
    end: BigInt,
    registers: Seq[Register]
) {
  def address(reg: Register): BigInt = base + reg.offset
  def contains(addr: BigInt): Boolean = addr >= base && addr <= end
}

/** A field together with where it lives: block instance, register and absolute address.
  *
  * This is the flat view most of the generator wants to iterate over.
  */
case class CsrField(block: BlockInstance, register: Register, field: Field) {
  def address: BigInt = block.address(register)

  /** Flat port name, matching the Python generator: `block_reg` or `block_reg_field`. */
  def ioName: String =
    (Seq(block.name, register.name) ++ field.name).mkString("_")

  /** The ports this field needs towards the IP block, as (name, Chisel type).
    *
    * Follows the README: `rw` -> Output data, `ro` -> Input data,
    * `wotrg` -> Output data + Output trg, `rotrg` -> Input data + Output trg,
    * `const` -> nothing. Usable directly with [[help.DynamicBundle]].
    */
  def ports: Seq[(String, Data)] = {
    val data = if (field.width == 1) Bool() else UInt(field.width.W)
    field.typ match {
      case FieldType.RW    => Seq(ioName -> Output(data))
      case FieldType.RO    => Seq(ioName -> Input(data))
      case FieldType.WoTrg => Seq(ioName -> Output(data), s"${ioName}_trg" -> Output(Bool()))
      case FieldType.RoTrg => Seq(ioName -> Input(data), s"${ioName}_trg" -> Output(Bool()))
      case FieldType.Const => Seq()
    }
  }
}

/** The whole system: all block instances of the memory map. */
case class SocSpec(blocks: Seq[BlockInstance]) {

  /** Every field of every register of every block instance, in spreadsheet order. */
  def fields: Seq[CsrField] =
    for {
      b <- blocks
      r <- b.registers
      f <- r.fields
    } yield CsrField(b, r, f)

  /** Every register with its absolute address. */
  def registers: Seq[(BlockInstance, Register, BigInt)] =
    for {
      b <- blocks
      r <- b.registers
    } yield (b, r, b.address(r))

  /** All IP-side ports of the adapter, in a stable order. */
  def ports: Seq[(String, Data)] = fields.flatMap(_.ports)

  def readableAddresses: Seq[BigInt] =
    registers.collect { case (_, r, a) if r.swReadable => a }.distinct.sorted

  def writableAddresses: Seq[BigInt] =
    registers.collect { case (_, r, a) if r.swWritable => a }.distinct.sorted

  override def toString: String =
    blocks.map { b =>
      val regs = b.registers.map { r =>
        val fs = r.fields.map { f =>
          val n = f.name.getOrElse("<whole>")
          val i = f.init.map(v => s" = 0x${v.toString(16)}").getOrElse("")
          f"      $n%-10s ${f.typ.name}%-5s [${f.rangeString}]$i"
        }
        (f"    0x${b.address(r).toString(16)}%8s ${r.name}" +: fs).mkString("\n")
      }
      (s"  ${b.name}: ${b.blockType} [0x${b.base.toString(16)} - 0x${b.end.toString(16)}]" +: regs)
        .mkString("\n")
    }.mkString("SocSpec\n", "\n", "\n")
}

object SocSpec {

  val DataWidth = 32

  /** Load and validate a CSR specification from an Excel file. */
  def load(path: String): SocSpec = fromSheets(Sheet.load(path))

  /** Build the spec from already loaded sheets (handy for tests). */
  def fromSheets(sheets: Map[String, Sheet]): SocSpec = {
    val map = sheets.getOrElse("Map", fail("Spreadsheet has no 'Map' sheet"))

    val blocks = map.rows.filter(row => cell(map, row, "Name").nonEmpty).map { row =>
      val name = cell(map, row, "Name")
      val blockType = cell(map, row, "Block")
      val base = parseNumber(cell(map, row, "Base Address"))
        .getOrElse(fail(s"Block '$name': missing Base Address"))
      val end = parseNumber(cell(map, row, "End Address"))
        .getOrElse(fail(s"Block '$name': missing End Address"))
      val sheet = sheets.getOrElse(
        blockType,
        fail(s"Block '$name' has type '$blockType', but there is no sheet called '$blockType'")
      )
      BlockInstance(name, blockType, base, end, parseRegisters(blockType, sheet))
    }

    val spec = SocSpec(blocks)
    validate(spec)
    spec
  }

  /** Parse the register sheet of one block type. Rows of the same register are grouped. */
  def parseRegisters(blockType: String, sheet: Sheet): Seq[Register] = {
    val rows = sheet.rows.filter(row => cell(sheet, row, "Register").nonEmpty)
    val regNames = rows.map(cell(sheet, _, "Register")).distinct

    regNames.map { regName =>
      val ctx = s"$blockType.$regName"
      val regRows = rows.filter(cell(sheet, _, "Register") == regName)

      val offsets = regRows.map(r => parseNumber(cell(sheet, r, "Offset"))).distinct
      if (offsets.size != 1 || offsets.head.isEmpty)
        fail(s"$ctx: all rows of a register need the same, non-empty Offset")

      val fields = regRows.map { r =>
        val fieldName = Option(cell(sheet, r, "Field")).filter(_.nonEmpty)
        val fctx = ctx + fieldName.map("." + _).getOrElse("")
        val typ =
          try FieldType.parse(cell(sheet, r, "Type"))
          catch { case e: IllegalArgumentException => fail(s"$fctx: ${e.getMessage}") }
        val (hi, lo) = parseRange(cell(sheet, r, "Range"), fctx)
        val init = parseNumber(cell(sheet, r, "Init"))
        Field(fieldName, typ, hi, lo, init)
      }

      Register(regName, offsets.head.get, fields)
    }
  }

  // ---------------------------------------------------------------------------
  // validation

  def validate(spec: SocSpec): Unit = {
    val names = spec.blocks.map(_.name)
    names.diff(names.distinct).distinct.foreach(n => fail(s"Block name '$n' is used more than once"))

    spec.blocks.foreach { b =>
      if (b.end < b.base) fail(s"Block '${b.name}': End Address is below Base Address")
      if (b.base % 4 != 0) fail(s"Block '${b.name}': Base Address must be 4-byte aligned")
    }

    // block address ranges must not overlap
    spec.blocks.sortBy(_.base).sliding(2).foreach {
      case Seq(a, b) if b.base <= a.end =>
        fail(s"Blocks '${a.name}' and '${b.name}' have overlapping address ranges")
      case _ =>
    }

    spec.blocks.foreach { b =>
      val regNames = b.registers.map(_.name)
      if (regNames.distinct.size != regNames.size)
        fail(s"Block type '${b.blockType}' has duplicate register names")

      val offsets = b.registers.map(_.offset)
      offsets.diff(offsets.distinct).distinct.foreach { o =>
        fail(s"Block type '${b.blockType}': several registers at offset 0x${o.toString(16)}")
      }

      b.registers.foreach { r =>
        val ctx = s"${b.blockType}.${r.name}"
        if (r.offset % 4 != 0) fail(s"$ctx: Offset must be 4-byte aligned")
        if (b.address(r) + 3 > b.end)
          fail(s"$ctx: register at 0x${b.address(r).toString(16)} lies outside block '${b.name}'")

        val unnamed = r.fields.count(_.name.isEmpty)
        if (unnamed > 0 && r.fields.size > 1)
          fail(s"$ctx: a register with several fields needs a name for each field")

        val fieldNames = r.fields.flatMap(_.name)
        if (fieldNames.distinct.size != fieldNames.size) fail(s"$ctx: duplicate field names")

        r.fields.foreach(f => validateField(s"$ctx${f.name.map("." + _).getOrElse("")}", f))

        // fields must not overlap, unless one is software-read-only and the other
        // software-write-only (e.g. a txData/rxData pair sharing the same bits)
        r.fields.combinations(2).foreach { case Seq(a, c) =>
          val sameDirection =
            (a.typ.swReadable && c.typ.swReadable) || (a.typ.swWritable && c.typ.swWritable)
          if ((a.mask & c.mask) != 0 && sameDirection)
            fail(s"$ctx: fields '${a.name.getOrElse("")}' and '${c.name.getOrElse("")}' overlap")
        }
      }
    }
  }

  private def validateField(ctx: String, f: Field): Unit = {
    if (f.lo < 0 || f.hi >= DataWidth || f.hi < f.lo)
      fail(s"$ctx: Range ${f.rangeString} is not inside 31:0")
    f.init.foreach { v =>
      if (v < 0 || v.bitLength > f.width)
        fail(s"$ctx: Init 0x${v.toString(16)} does not fit in ${f.width} bits")
    }
    if (f.typ == FieldType.Const && f.init.isEmpty)
      fail(s"$ctx: a const field needs an Init value")
  }

  // ---------------------------------------------------------------------------
  // cell parsing

  /** Value of a named column in a row, trimmed. */
  private def cell(sheet: Sheet, row: Seq[String], column: String): String = {
    val idx = sheet.header.indexOf(column)
    if (idx == -1) fail(s"Missing column '$column'")
    Option(row(idx)).getOrElse("").trim
  }

  /** Parse a spreadsheet number. Accepts `0x..` hex, decimal, and the `0.0` form
    * Apache POI produces for numeric cells. Empty and `?` mean "no value".
    */
  def parseNumber(s: String): Option[BigInt] = {
    val t = Option(s).getOrElse("").trim.toLowerCase.replace("_", "")
    if (t.isEmpty || t == "?") None
    else if (t.startsWith("0x")) Some(BigInt(t.drop(2), 16))
    else
      try {
        val d = BigDecimal(t)
        if (!d.isWhole) fail(s"'$s' is not a whole number")
        Some(d.toBigInt)
      } catch { case _: NumberFormatException => fail(s"'$s' is not a number") }
  }

  /** Parse `hi:lo` (or a single bit `n`). */
  def parseRange(s: String, ctx: String): (Int, Int) =
    s.split(":").map(_.trim) match {
      case Array(hi, lo) if hi.forall(_.isDigit) && lo.forall(_.isDigit) && hi.nonEmpty && lo.nonEmpty =>
        (hi.toInt, lo.toInt)
      case Array(bit) if bit.nonEmpty && bit.forall(_.isDigit) => (bit.toInt, bit.toInt)
      case _ => fail(s"$ctx: Range '$s' is not of the form hi:lo")
    }

  private def fail(msg: String): Nothing = throw new IllegalArgumentException(s"CSR spec: $msg")
}
