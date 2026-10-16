# AGC — an Apollo Guidance Computer assembler and virtual hardware in VB.NET

A VB.NET (.NET 10) implementation of a yaYUL-style assembler for the Apollo
Guidance Computer, together with a Block II virtual hardware emulator that can
execute the assembled rope image.

The project lives in `src/AGC/AGC.vbproj` and builds a single command-line
program named `AGC`.

```
C:\Users\Administrator\Downloads\Apollo-11\Apollo-11\src\AGC>dotnet build
C:\Users\Administrator\Downloads\Apollo-11\Apollo-11\src\AGC>dotnet bin\Debug\net10.0\AGC.dll selftest
```

## Commands

| Command | Purpose |
|---|---|
| `AGC assemble <dir> [--symbols FILE] [--all-errors] [--max-lines]` | Assemble an AGC program (the directory must contain `MAIN.agc`) and report statistics and diagnostics. |
| `AGC run <dir> [--seconds N] [--force] [--find-zero] [--trace-boot] [--bank-probe]` | Assemble and execute on the virtual hardware. `--force` runs even when assembly reported errors. |
| `AGC selftest` / `AGC smoke` | Run the end-to-end test suite (39 checks). |
| `AGC compare <dir> <binsource>` | Compare the assembled rope against a reference `.binsource`. |
| `AGC dump <dir> --out FILE` | Write the rope image in virtualagc `.binsource` format. |
| `AGC info <dir>` | Source statistics without assembling. |

## Architecture

```
src/AGC/
  Core/       Word arithmetic, pseudo-address codec, symbol table, diagnostics,
              rope image, and the end-to-end test suite (SmokeTest.vb).
  Assembly/   Source reader ($ includes, line splitting), the two-pass assembler,
              the opcode table, pseudo-op handling, instruction encoders, and the
              interpretive (Danzig) instruction encoding.
  Hardware/   AgcMemory (bank switching), AgcCpu (the Block II instruction set),
              AgcIo (channels, timers, scaler), AgcDevices (DSKY, CDU, downlink),
              and AgcMachine which ties them together.
  Runtime/    AgcInterpreter, the interpretive instruction engine, and Runner,
              which wires the command line to the hardware.
```

The two authoritative data structures are:

* `Assembly/OpcodeTable.vb` — the mnemonic table. Each entry's `Opcode` field is
  the assembled instruction field, and `Opcode >> 9` must equal the dispatch
  value the CPU's `Select Case` matches. `smoke` audits all 38 basic and extended
  mnemonics against that contract.
* `Hardware/AgcCpu.vb` — the instruction executor, whose `Select Case` is the
  other half of that contract.

### AGC numbering conventions used throughout

The AGC's own numbers are octal, and several places in the codebase depend on
distinguishing them from decimal. The important ones:

* A word is 15 data bits plus a parity bit; an assembled *instruction* needs 16
  bits because the opcode occupies the top bits. `AgcWord.Mask15` masks data,
  `AgcWord.Mask17` keeps instruction words intact.
* Erasable memory: 2048 words in eight 256-word banks. `0000..01377` is
  unswitched; `01400..03777` is switched and encodes the E-bank in bits 8-9.
* Fixed memory: 36 banks of 1024 words. Banks `02` and `03` are the unswitched
  "fixed-fixed" area at `04000..07777`; the rest are switched through FBANK and
  BBANK. Banks `030..043` form the super-bank module, placed 02000 lower in the
  pseudo-address space.
* `AgcAddress.PseudoValue` flattens a segmented address back to the assembler's
  pseudo address. `AgcCodec.PseudoToAddress` inverts it. The smoke test asserts
  that **all 32,768 pseudo addresses round trip exactly** — this invariant caught
  several genuine bugs and should be kept green.

## What is verified working

`AGC selftest` runs 39 assertions, all passing:

* **Assembler encoding.** `CA`, `AD`, `TS`, `INCR`, `CCS`, `TCF` and the rest of
  the basic set encode to the words the CPU expects, verified against
  hand-computed values and against the reference `binsource`.
* **Opcode/dispatch contract.** All 38 basic and extended mnemonics agree with
  the CPU's dispatch values; zero mismatches.
* **Pseudo-address codec.** All 32,768 pseudo addresses convert and invert
  exactly; none are rejected.
* **CPU execution.** A small AGC program assembles, runs, and computes
  `1 + 2 = 3` through `CA`/`AD`/`TS`/`INCR`/`CCS`/`TCF`.
* **Interpretive (Danzig) engine.** `DLOAD`/`DAD`/`DSU`/`DCOMP`/`ABS`/`EXIT`
  execute correctly with 28-bit one's-complement MPAC arithmetic.
* **Vector and matrix operations.** `DOT` computes a correct dot product, with
  15-bit components.
* **Equates.** Forward references (`FLAGWRD5 = STATE +5`, 637 lines before
  `STATE` is defined), whitespace-separated offsets, and slashed names (`J/M`).
* **Dual-opcode lines.** `SET BON` packs both opcodes into one word.
* **Super-bank section naming.** `BANK n` + bare `EQUALS` + `SETLOC <label>`.
* **Bank 2 of Comanche055 matches the reference `binsource` word for word** at
  offsets 0-7, which independently confirms the instruction encoding.

## What does not work — known limitations

**The rope does not complete a boot.** This is the principal limitation. The
assembled Comanche055 image loads, starts at `04000`, and executes 85,344
instructions per simulated second at correct cycle timing with no restarts — but
the program counter reaches `000000` after 11 instructions:

```
04000 INHINT   04001 CAF GOBB   04002 XCH BBANK   04003 TCF GOPROG
```

Two defects are identified and reproducible but not fixed:

1. **`TCF` at `04003` transfers nowhere.** The smoke test's `TCF` and the rope's
   `TCF` carry the **identical opcode** (`024011` versus `024526`, both dispatch
   value 20), and the smoke test's branches correctly. So this is *not* a missing
   or wrong case for `TCF`:

   * No case range in `AgcCpu.vb` covers dispatch value 20 (there is a gap:
     `...&H16-&H17`, `&H18-&H1F`, `&H20-&H27`).
   * The rope's `TCF` nevertheless does **not** reach `Case Else`. An
     unconditional trace in the fallback, restricted to the boot region, never
     fired.
   * A trace at the top of each case group shows no case body executing for it
     either.

   So the instruction is dispatched, matches nothing, does not raise the
   unmatched-opcode signal, and leaves `NextZ` untouched so control advances to
   `04004`. That combination is inconsistent with a plain `Select Case`, and the
   task is to find what sits between the dispatch and the case selection.
   Note also that `TCF` and `DAS` both encode as `020000`, which the CPU cannot
   disambiguate — the assembler and CPU use two different opcode conventions that
   need one coordinated reconciliation.

2. **`XCH BBANK` does not take effect.** The instruction writes
   `erasable(6) = 012063` correctly — verified by direct read immediately
   afterwards — yet the bank refresh in the *same* `FinishInstruction` call reads
   `bbankReg = 000000`. A write and a read of the same address, immediately
   adjacent, disagreeing. This is unexplained.

**Assembly is not clean.** `AGC assemble` reports 729 errors for Comanche055 and
648 for Luminary099 (down from 12,015 and 11,235). Because repeated identical
messages are capped at four, the *displayed* count understates the distinct
sites; pass `--all-errors --max-lines` to see the full list. The remaining
classes are: interpretive operands that reference an inline label defined among
the operands (`SL1`, `BOVB`), `EBANK=` destinations that are not erasable, and
malformed lines in the sources.

**The interpretive instruction set is incomplete.** Roughly 35 of about 120
opcodes are implemented — scalar and double arithmetic, shifts, push-down, basic
branches, and the vector/matrix entry points. Missing notably: `MXV`'s
execution, `VPROJ`, the `*`-suffixed variants, `NORM`'s full semantics, and the
0162 flag family's operand encoding (`SET`/`BON`/`BOF`/`CLEAR` are recognised but
their flag-and-polarity bits are not encoded). **No interpretive instruction has
ever executed in a real boot**, because the boot fails first.

**The 0162 flag family shares one opcode** across eighteen mnemonics, separated
by bits in the operand word. `AGC` recognises them; it does not yet encode them.

## A note on the reference `binsource`

The `Comanche055.binsource` from virtualagc is **not the same ROM revision** as
the sources transcribed in this repository. Its bank 2 at `04000` does not
contain the interrupt lead-ins that `INTERRUPT_LEAD_INS.agc` defines there, and
its bank 3 is empty. It is useful for spot-checking individual encodings (and
bank 2 does match at offsets 0-7), but it cannot be used as a word-for-word
oracle for the whole rope, and a 100% match should not be expected.

## Reading the assembler's diagnostics

Two switches exist because the default output is capped:

* `--all-errors` raises the per-message duplicate cap so every site is listed.
* `--max-lines` raises the total line limit.
* `--resolve-trace` reports each symbol that fails to resolve, with the symbol's
  stored address, the offsets applied, and the pseudo address computed. This is
  the tool that localised the equate and super-bank bugs.

## Suggested next steps

1. Reconcile the assembler's opcode table with the CPU's `Select Case` in one
   coordinated change, deriving each case value empirically from an instruction
   that is known to work rather than by reasoning about the manual. The `TCF`
   case is the first to fix.
2. Explain the `XCH BBANK` write/read disagreement — a small, self-contained
   question with a reproducible trace.
3. Once the boot advances past the interrupt lead-ins, the executive, timers and
   DSKY will be exercised for the first time, and the interpretive engine will
   finally run.
