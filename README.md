# Agile HW Design - Lab 4: CSR Adapter Generator

A Chisel generator that turns a spreadsheet description of Control and Status
Registers into an APB-attached CSR adapter.

`lab4/soc.xlsx` is the source of truth: a `Map` sheet with the memory map plus
one sheet per block type listing registers, fields, access types, bit ranges
and reset values. From that, `CsrAdapter` builds the APB target logic and the
IP-side ports at elaboration time.

The lab also ships a Python reference generator (`lab4/csr_adapter_gen.py`)
that emits the same adapter as flat SystemVerilog; two test suites use it as an
independent oracle. `lab4/README.md` is the original lab description.

## Layout

| Path | What it is |
|---|---|
| `lab4/src/main/scala/CsrSpec.scala` | Spreadsheet to validated Scala data model (`SocSpec`, `BlockInstance`, `Register`, `Field`, `FieldType`, `CsrPort`) |
| `lab4/src/main/scala/Apb.scala` | `ApbPort` bundle and `ApbTarget`, the zero-wait-state APB handshake |
| `lab4/src/main/scala/CsrAdapter.scala` | The generator: IO construction, address decoding, per-field logic |
| `lab4/src/main/scala/help/` | Provided helpers: `Sheet` (POI Excel loader), `DynamicBundle` |
| `lab4/src/test/scala/` | Test suites |
| `.github/workflows/test.yml` | CI: Verilator + Python, reference adapter, `sbt test` |

## How it works

**Specification.** `SocSpec.load("soc.xlsx")` builds one `BlockInstance` per
map row and groups register rows into `Register`s with `Field`s. Numbers accept
hex, decimal and the `0.0` form POI produces; `?` means "no value". Everything
is validated on load - unknown types, ranges outside `31:0`, init values that
don't fit, a `const` without init, unaligned or duplicate offsets, registers
outside their block, overlapping blocks, overlapping fields in the same access
direction - so the generator needs no defensive checks.

**Bus.** `ApbTarget` derives the setup and access phases from `psel`/`penable`
and ties `pready` high, so a transfer is exactly two cycles and `read`/`write`
are one-cycle strobes, usable directly as write enables and `trg` pulses.

**Generation.** IP-side ports come from `spec.csrBundle()`, a nested
`DynamicBundle`, looked up per field via `CsrPort.lookup`. Decoding compares
`paddr` against the readable and writable address lists, so a write to a
read-only register and an unmapped address both answer with `pslverr`. Read
data is an OR of per-field terms, each masked by its address match and shifted
into place. State is one register per software-writable field; everything else
is combinational.

### Field types

| Type | Software | IP-side ports |
|---|---|---|
| `rw` | read/write | `block.reg(.field)` - output, driven from an internal register |
| `ro` | read | `block.reg(.field)` - input from the block |
| `wotrg` | write | `.data` output + `.trg` output, pulsed on write |
| `rotrg` | read | `.data` input + `.trg` output, pulsed on read |
| `const` | read | none; value hardwired in the adapter |

In `soc.xlsx` this gives `csr.uart0.ctrl.en`, `csr.uart0.data.txData.trg`,
`csr.gpio1.dir` and so on; `sysInfo` has no ports, as its only field is a
`const`.

## Usage

Needs a JDK and sbt (Scala 2.13, Chisel 6.7.0, chiseltest 6.0.0 and Apache POI
are pulled in by sbt). Verilator is needed only for the simulation tests - the
lab description recommends **5.48**, since 5.50+ does not work with chiseltest.
Python 3 with `pip install -r requirements.txt` is needed only for the
reference generator.

```bash
cd lab4

sbt test                              # run the test suite
sbt run                               # elaborate -> generated/CsrAdapter.sv
python3 csr_adapter_gen.py soc.xlsx   # reference adapter -> soc_adapter.sv
```

`sbt run` also prints the parsed specification, the quickest way to check how a
spreadsheet change was understood. Another `.xlsx` can be passed to the
`CsrAdapter` constructor (the `main` hardcodes `soc.xlsx`); it needs the same
sheet structure.

## Tests

| Suite | Covers | Needs |
|---|---|---|
| `SocSpecTest` | Parsing of `soc.xlsx`, number/range helpers, validation rules | - |
| `ApbTargetTest` | APB handshake: write/read-back, errors, one strobe per transfer, phases | - |
| `CsrLogicTest` | Adapter behaviour per field type: resets, partial writes, triggers, rejects | - |
| `ReferenceSpecTest` | Parsed spec vs. generated `soc_adapter.sv`: ports, addresses, field placement, constants, resets, triggers | Python |
| `ApbReferenceTest` | 500 randomized transfers (fixed seed) compared against the reference adapter | Python + Verilator |
| `GeneratorTest` | The handout's testbench: our adapter, and the reference adapter as a blackbox | Verilator (second case) |

The reference suites cancel themselves when Python or Verilator is missing, but
fail when `CI=true`, so CI cannot pass without running them.
