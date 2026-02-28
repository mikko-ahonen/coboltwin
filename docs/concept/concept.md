# COBOLTWIN -- Concept

## Problem

Organizations migrating legacy COBOL systems to modern languages (Java, Python, etc.)
need confidence that the new implementation produces **identical results** to the
original. Manual testing is error-prone and does not scale. Differences in data type
handling, rounding, string padding, and control flow between COBOL and modern languages
make silent regressions likely.

## Idea

Run the original COBOL program and its Java re-implementation **side by side** against
the same input data, then **automatically compare** the outputs record-by-record and
field-by-field. Any difference is flagged immediately, giving developers fast feedback
during the migration.

The COBOL source is **preprocessed** to strip mainframe dependencies (DB2 SQL, IMS/DL-I
calls) and replace them with stubs, making the program runnable on Linux with GnuCOBOL.
Both sides emit JSON Lines to stdout so outputs can be compared automatically.

## Scope

### In scope

- Preprocessing COBOL source to make it runnable on Linux (strip DB2 SQL, replace
  DL/I calls with stubs, convert COMP-3 to DISPLAY).
- A test wrapper that feeds flat-file data into the COBOL program and captures output
  as JSON Lines.
- A Java project that re-implements the same business logic and produces matching
  JSON Lines output.
- A Python comparison harness that diffs the two outputs and produces a clear report.
- Support for multiple test cases (different input files, edge cases).

### Out of scope (for now)

- Full DB2 or IMS/DL-I emulation -- we use stubs and mock data instead.
- Automated COBOL-to-Java code translation -- the Java side is written by developers.
- Production deployment of either side -- this is a testing/validation tool.
- GUI or web interface -- CLI-based workflow.

## Success Criteria

- Given the same input file, both COBOL and Java produce identical JSON Lines output.
- When a difference exists, the comparison report identifies the exact row and field.
- Adding a new test case requires only a new input data file, not code changes.
