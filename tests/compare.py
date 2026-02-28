#!/usr/bin/env python3
"""Twin comparison harness: runs COBOL exe and Java JAR, diffs JSON Lines field by field."""

import json
import subprocess
import sys

COBOL_EXE = "build/custproc"
JAVA_JAR  = "custproc-java/target/custproc-java-0.1.0.jar"
JAVA_CMD  = ["java", "-jar", JAVA_JAR]
FIELDS    = ["cust_id", "name", "balance", "status", "segment"]


def run(cmd):
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"FAIL: {' '.join(cmd)} exited {result.returncode}", file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        sys.exit(1)
    return result.stdout.strip().splitlines()


def main():
    cobol_lines = run([COBOL_EXE])
    java_lines  = run(JAVA_CMD)

    if len(cobol_lines) != len(java_lines):
        print(f"FAIL: record count mismatch: COBOL={len(cobol_lines)}, Java={len(java_lines)}")
        sys.exit(1)

    total_fields = 0
    mismatches = 0

    for i, (cl, jl) in enumerate(zip(cobol_lines, java_lines), 1):
        crec = json.loads(cl)
        jrec = json.loads(jl)
        for field in FIELDS:
            total_fields += 1
            cv = crec.get(field, "")
            jv = jrec.get(field, "")
            if cv != jv:
                mismatches += 1
                print(f"  MISMATCH record {i} field '{field}': COBOL={cv!r} Java={jv!r}")

    records = len(cobol_lines)
    if mismatches == 0:
        print(f"OK: All {records} records match across {len(FIELDS)} fields")
    else:
        print(f"FAIL: {mismatches} field mismatch(es) in {records} records")
        sys.exit(1)


if __name__ == "__main__":
    main()
