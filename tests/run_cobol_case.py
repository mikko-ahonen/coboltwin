#!/usr/bin/env python3
import json
import subprocess
import sys
from pathlib import Path

def run(exe: str, db2_in: str, db2_out: str):
    Path("db2_in.dat").write_text(Path(db2_in).read_text(encoding="utf-8"), encoding="utf-8")
    p = subprocess.run([exe], capture_output=True, text=True)
    if Path("db2_out.dat").exists():
        Path(db2_out).write_text(Path("db2_out.dat").read_text(encoding="utf-8"), encoding="utf-8")
    jsonl = []
    for line in p.stdout.splitlines():
        line = line.strip()
        if line.startswith("{") and line.endswith("}"):
            try:
                jsonl.append(json.loads(line))
            except Exception:
                pass
    return {"returncode": p.returncode, "stdout": p.stdout, "stderr": p.stderr, "jsonl": jsonl}

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: run_cobol_case.py <exe> <db2_in.dat> <db2_out.dat>")
        sys.exit(2)
    print(json.dumps(run(sys.argv[1], sys.argv[2], sys.argv[3]), indent=2))
