#!/usr/bin/env python3
import json
import subprocess
import sys
from pathlib import Path

def run(exe: str, in_file: str, out_file: str):
    env = dict(**{k:v for k,v in dict().items()})
    # The stubbed COBOL uses fixed filenames db2_in.dat / db2_out.dat in cwd.
    # We copy/point them here for a predictable interface.
    Path("db2_in.dat").write_text(Path(in_file).read_text(encoding="utf-8"), encoding="utf-8")

    p = subprocess.run([exe], capture_output=True, text=True)
    # Program writes db2_out.dat + emits JSON Lines to stdout
    if Path("db2_out.dat").exists():
        Path(out_file).write_text(Path("db2_out.dat").read_text(encoding="utf-8"), encoding="utf-8")

    return {
        "returncode": p.returncode,
        "stdout": p.stdout,
        "stderr": p.stderr,
        "jsonl": [json.loads(line) for line in p.stdout.splitlines() if line.strip().startswith("{")],
    }

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: run_cobol_case.py <exe> <db2_in.dat> <db2_out.dat>")
        sys.exit(2)
    result = run(sys.argv[1], sys.argv[2], sys.argv[3])
    print(json.dumps(result, indent=2))
