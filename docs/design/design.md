# COBOLTWIN -- Design

## Overview

COBOLTWIN validates COBOL-to-Java migrations by running both implementations against
identical test data and comparing outputs field by field. See
[concept](../concept/concept.md) for background.

## Architecture

```
                  +--------------+     +-----------+
  COBOL source -->| Preprocessor |---->| COBOL exe |---> COBOL output (JSON Lines)
                  +--------------+     +-----------+              |
                   strip DB2 SQL,            ^                    v
                   stub DL/I calls      test data            +---------+
                                             |               | Compare |---> diff report
                                             |               +---------+
                                             |          +-----------+  ^
                                             +--------->|  Java app |--+
                                                        +-----------+
                                                   Java output (JSON Lines)
```

## Components

| Component            | Language | Purpose                                        |
|----------------------|----------|------------------------------------------------|
| COBOL preprocessor   | Java     | Strip DB2 SQL, replace DL/I calls, fix types   |
| Test wrapper         | COBOL    | File I/O harness around the legacy program     |
| DL/I stub            | COBOL    | Returns canned data in place of IMS calls      |
| Java re-implementation | Java   | Same business logic, same JSON output schema   |
| Comparison harness   | Python   | Run both sides, diff outputs, produce report   |

### COBOL Preprocessor

Java tool using ProLeap COBOL Parser (ANTLR4-based). Transforms mainframe COBOL into
GnuCOBOL-runnable source:

- Strip `EXEC SQL ... END-EXEC` blocks (DB2 embedded SQL)
- Replace `CALL 'CBLTDLI'` with `CALL 'DLI-STUB'` (IMS/DL-I)
- Convert `COMP-3` (packed decimal) to `DISPLAY` for flat-file mocking

**Why ProLeap** -- It provides a full COBOL preprocessor that handles COPY/REPLACE
directives, giving us a proper AST rather than fragile regex-only transforms.

Location: `tools/cobol-preprocessor/`

### Test Wrapper

A free-format COBOL program (`src/TEST-WRAPPER.cbl`) that:

- Reads fixed-width records from `db2_in.dat`
- Moves fields into working storage and calls the core program
- Writes output records to `db2_out.dat`
- Emits JSON Lines to stdout (one object per row)

**Why a wrapper** -- COBOL's FILE SECTION/FD declarations are structurally rigid.
Injecting file I/O into an arbitrary program is brittle. A separate wrapper keeps the
core program unchanged.

### DL/I Stub

Minimal COBOL subprogram (`src/DLI-STUB.cbl`) that returns canned segment data via
LINKAGE SECTION. The preprocessor rewrites DL/I calls to invoke this stub.

### Java Re-implementation

A Java project that implements the same business logic as the COBOL program. Must:

- Read the same fixed-width input format
- Apply the same validation/transformation rules
- Emit JSON Lines to stdout using the same field names and schema

### Comparison Harness

Python script that orchestrates a test run:

1. Run the COBOL executable, capture JSON Lines output
2. Run the Java program against the same input, capture JSON Lines output
3. Compare outputs record by record, field by field
4. Report mismatches: row number, field name, COBOL value, Java value

## Output Format

Both sides emit **JSON Lines** (one JSON object per input record) to stdout.

```json
{"cust_id":"C001      ","name":"ALICE SMITH                    ","balance":"0012350.00","status":"VALID     ","segment":"RETAIL    "}
```

Field values preserve COBOL formatting (fixed-width padding, sign conventions) so the
comparison is exact. The harness can optionally normalize values (trim spaces, align
decimal precision) when strict byte-level matching is too rigid.

## Build and Run

```
make          # preprocess COBOL, compile with GnuCOBOL
make run      # run the COBOL side
pytest        # run comparison harness
```

Prerequisites: JDK 17+, Maven 3+, GnuCOBOL (`cobc`), Python 3.

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| COMP-3 / zoned decimal formatting differences | Preprocessor converts to DISPLAY; harness normalizes numerics |
| DL/I stub returns unrealistic data | Stubs are per-test-case configurable, not hardcoded |
| GnuCOBOL vs IBM COBOL edge cases (rounding, overflow) | Document known differences; add targeted test cases |
| Insufficient test coverage for large programs | Branch-coverage analysis on COBOL source to guide test data |

## Current State and Next Steps

**Done:**
- COBOL preprocessor (`tools/cobol-preprocessor/`)
- Test wrapper and DL/I stub (`src/TEST-WRAPPER.cbl`, `src/DLI-STUB.cbl`)
- Basic Python runner (`tests/run_cobol_case.py`)

**Next:**
- Java re-implementation of CUSTPROC business logic
- Enhanced Python harness with side-by-side execution and field-level diff
- Test data sets covering normal, edge, and error cases
