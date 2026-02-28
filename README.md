# COBOLTWIN

COBOLTWIN helps validate that a modern implementation (e.g., Java) matches legacy COBOL behavior by running both against identical test data.

This project uses:
- Java (for COBOL preprocessing / parsing)
- Python (for test orchestration and result comparison)
- GnuCOBOL (for compiling and running the stubbed COBOL on Linux)

## Java preprocessor

The preprocessor is implemented using ProLeap COBOL Parser (ANTLR4-based parser that generates AST/ASG and includes a COBOL preprocessor for COPY/REPLACE/PROCESS).  
Repo: https://github.com/uwol/proleap-cobol-parser

### What it outputs

It produces `build/CUSTPROC_CORE.cbl`:
- Embedded DB2 `EXEC SQL ... END-EXEC` removed (or stripped defensively)
- `COMP-3` converted to `DISPLAY` to make mock files easy
- `CALL 'CBLTDLI' USING WS-SEGMENT` replaced with `CALL 'DLI-STUB' USING WS-SEGMENT`

## Wrapper approach (recommended)

COBOL is structurally rigid; injecting FILE SECTION/FDs into an arbitrary program is brittle.

Instead:
- `src/TEST-WRAPPER.cbl` does Linux file I/O
- It moves fields into WS variables
- Calls `CUSTPROC` (the core program)
- Writes output records + emits JSON Lines to stdout

## Build

Prereqs: JDK 17+, Maven 3+, GnuCOBOL (`cobc`)

```bash
make
```

## Run

```bash
cp tests/data/db2_in.dat .
make run
```

Outputs:
- `db2_out.dat`
- JSON objects on stdout (one per input row)

## Python harness

```bash
python3 tests/run_cobol_case.py build/custproc tests/data/db2_in.dat build/db2_out.dat
```
