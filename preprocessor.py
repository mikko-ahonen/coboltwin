#!/usr/bin/env python3
"""
COBOL mainframe -> GnuCOBOL-friendly preprocessor.

Transforms:
- EXEC SQL INCLUDE SQLCA END-EXEC.  -> removed; defines minimal SQLCODE
- EXEC SQL DECLARE <c> CURSOR FOR SELECT ... END-EXEC. -> captured for stub generation
- EXEC SQL OPEN <c> END-EXEC.       -> PERFORM DB2-OPEN
- EXEC SQL FETCH <c> INTO :vars ... -> PERFORM DB2-FETCH
- EXEC SQL UPDATE ... SET ... WHERE -> PERFORM DB2-UPDATE (writes to file)
- CALL 'CBLTDLI' ...                -> CALL 'DLI-STUB'
- PIC ... COMP-3                    -> PIC ... DISPLAY

Adds:
- FILE-CONTROL + FILE SECTION for DB2 mock input/output files
- DB2 stub paragraphs to read/write fixed-width records
- JSON Lines emitter (one JSON object per processed row)
- Python harness script generator (run_cobol_case.py)

Usage:
  python3 preprocessor.py src/CUSTPROC.cbl build/CUSTPROC_linux.cbl build
"""

from __future__ import annotations
import re
import sys
from pathlib import Path
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple


@dataclass
class VarDef:
    name: str
    pic: str  # full PIC clause line tail, normalized for GnuCOBOL


@dataclass
class SqlModel:
    cursor_name: Optional[str] = None
    select_into_vars: List[str] = None
    update_set_vars: List[str] = None
    update_where_vars: List[str] = None

    def __post_init__(self):
        self.select_into_vars = self.select_into_vars or []
        self.update_set_vars = self.update_set_vars or []
        self.update_where_vars = self.update_where_vars or []


PIC_RE = re.compile(r"^\s*\d+\s+([A-Z0-9-]+)\s+PIC\s+([^.\n]+)\.", re.IGNORECASE)
COMP3_RE = re.compile(r"\bCOMP-3\b", re.IGNORECASE)

DECLARE_CURSOR_RE = re.compile(r"DECLARE\s+([A-Z0-9_]+)\s+CURSOR\s+FOR", re.IGNORECASE)
OPEN_CURSOR_RE = re.compile(r"^\s*EXEC\s+SQL\s+OPEN\s+([A-Z0-9_]+)", re.IGNORECASE)
CLOSE_CURSOR_RE = re.compile(r"^\s*EXEC\s+SQL\s+CLOSE\s+([A-Z0-9_]+)", re.IGNORECASE)
FETCH_RE = re.compile(r"FETCH\s+([A-Z0-9_]+)\s+INTO\s+(.+)", re.IGNORECASE)
UPDATE_RE = re.compile(r"UPDATE\s+([A-Z0-9_.]+)\s+SET\s+(.+?)\s+WHERE\s+(.+)", re.IGNORECASE)

INCLUDE_SQLCA_RE = re.compile(r"EXEC\s+SQL\s+INCLUDE\s+SQLCA", re.IGNORECASE)
EXEC_SQL_START_RE = re.compile(r"^\s*EXEC\s+SQL\b", re.IGNORECASE)
END_EXEC_RE = re.compile(r"END-EXEC\.", re.IGNORECASE)

CALL_CBLTDLI_RE = re.compile(r"CALL\s+'CBLTDLI'", re.IGNORECASE)

# naive host var capture :WS-FOO, :BAR
HOSTVAR_RE = re.compile(r":\s*([A-Z0-9-]+)", re.IGNORECASE)


def normalize_pic_tail(pic_tail: str) -> str:
    # Replace COMP-3 with DISPLAY and trim extra spaces
    pic_tail = COMP3_RE.sub("DISPLAY", pic_tail)
    pic_tail = re.sub(r"\s+", " ", pic_tail).strip()
    return pic_tail


def parse_working_storage_vars(lines: List[str]) -> Dict[str, VarDef]:
    vars_: Dict[str, VarDef] = {}
    for ln in lines:
        m = PIC_RE.match(ln)
        if not m:
            continue
        name = m.group(1).upper()
        pic_tail = normalize_pic_tail(m.group(2))
        vars_[name] = VarDef(name=name, pic=pic_tail)
    return vars_


def extract_host_vars(text: str) -> List[str]:
    # returns host vars in appearance order, uppercase
    return [v.upper() for v in HOSTVAR_RE.findall(text)]


def fixed_width_len_from_pic(pic_tail: str) -> int:
    """
    Rough length estimation for DISPLAY fields.
    Examples:
      X(10) => 10
      9(5)  => 5
      S9(7)V99 => 9 (treat V as digits)
      9(3)V9(2) => 5
    If can't parse, return 50.
    """
    t = pic_tail.upper()

    # If explicitly X(n)
    m = re.search(r"X\((\d+)\)", t)
    if m:
        return int(m.group(1))

    # Sum all 9(n) groups
    nums = [int(x) for x in re.findall(r"9\((\d+)\)", t)]
    base = sum(nums) if nums else 0

    # Add standalone 9's
    base += len(re.findall(r"(?<!\()9(?![\d\)])", t))

    # Account for V with following digits (very approximate)
    if "V" in t and base > 0:
        pass

    return base if base > 0 else 50


def gen_db2_file_defs(select_vars: List[str], var_defs: Dict[str, VarDef]) -> Tuple[str, str]:
    """
    Generates:
      - FD for DB2-IN-FILE record with fields matching SELECT INTO vars
      - FD for DB2-OUT-FILE record with fields matching updated fields (we’ll create later)
    Here we return IN-FD and IN-01 only; OUT is built separately.
    """
    fd_lines = []
    fd_lines.append("       FD  DB2-IN-FILE.\n")
    fd_lines.append("       01  DB2-IN-REC.\n")

    for v in select_vars:
        vd = var_defs.get(v)
        if vd:
            pic = vd.pic
        else:
            # fallback: string field
            pic = f"X({50})"
        # Ensure it's DISPLAY-friendly (no COMP/COMP-3)
        pic = COMP3_RE.sub("DISPLAY", pic)
        fd_lines.append(f"           05  IN-{v} PIC {pic}.\n")

    in_fd = "".join(fd_lines)
    in_move = []
    for v in select_vars:
        in_move.append(f"           MOVE IN-{v} TO {v}.\n")
    return in_fd, "".join(in_move)


def gen_db2_out_defs(all_out_vars: List[str], var_defs: Dict[str, VarDef]) -> Tuple[str, str]:
    fd_lines = []
    fd_lines.append("       FD  DB2-OUT-FILE.\n")
    fd_lines.append("       01  DB2-OUT-REC.\n")
    for v in all_out_vars:
        vd = var_defs.get(v)
        pic = vd.pic if vd else f"X({50})"
        pic = COMP3_RE.sub("DISPLAY", pic)
        fd_lines.append(f"           05  OUT-{v} PIC {pic}.\n")

    out_fd = "".join(fd_lines)
    out_move = []
    for v in all_out_vars:
        out_move.append(f"           MOVE {v} TO OUT-{v}.\n")
    return out_fd, "".join(out_move)


def inject_env_division_for_files(src_lines: List[str]) -> List[str]:
    """
    Adds FILE-CONTROL for DB2-IN-FILE and DB2-OUT-FILE if not already present.
    Tries to inject under ENVIRONMENT DIVISION / INPUT-OUTPUT SECTION / FILE-CONTROL.
    If not found, inserts a minimal ENVIRONMENT DIVISION skeleton near top.
    """
    text = "".join(src_lines).upper()
    if "SELECT DB2-IN-FILE" in text and "SELECT DB2-OUT-FILE" in text:
        return src_lines

    out = []
    inserted = False

    for i, ln in enumerate(src_lines):
        out.append(ln)
        if not inserted and re.search(r"^\s*FILE-CONTROL\.\s*$", ln, re.IGNORECASE):
            out.append("           SELECT DB2-IN-FILE ASSIGN TO 'db2_in.dat'\n")
            out.append("               ORGANIZATION IS LINE SEQUENTIAL.\n")
            out.append("           SELECT DB2-OUT-FILE ASSIGN TO 'db2_out.dat'\n")
            out.append("               ORGANIZATION IS LINE SEQUENTIAL.\n")
            inserted = True

    if inserted:
        return out

    # If no FILE-CONTROL exists, attempt to inject a minimal ENVIRONMENT DIVISION before DATA DIVISION
    out = []
    for ln in src_lines:
        if not inserted and re.search(r"^\s*DATA\s+DIVISION\.", ln, re.IGNORECASE):
            out.append("       ENVIRONMENT DIVISION.\n")
            out.append("       INPUT-OUTPUT SECTION.\n")
            out.append("       FILE-CONTROL.\n")
            out.append("           SELECT DB2-IN-FILE ASSIGN TO 'db2_in.dat'\n")
            out.append("               ORGANIZATION IS LINE SEQUENTIAL.\n")
            out.append("           SELECT DB2-OUT-FILE ASSIGN TO 'db2_out.dat'\n")
            out.append("               ORGANIZATION IS LINE SEQUENTIAL.\n")
            inserted = True
        out.append(ln)

    return out


def inject_file_section(src_lines: List[str], in_fd: str, out_fd: str) -> List[str]:
    """
    Inserts FD definitions into FILE SECTION if present; else creates FILE SECTION.
    """
    text = "".join(src_lines).upper()
    if "FD  DB2-IN-FILE" in text and "FD  DB2-OUT-FILE" in text:
        return src_lines

    out = []
    inserted = False

    for ln in src_lines:
        out.append(ln)
        if not inserted and re.search(r"^\s*FILE\s+SECTION\.\s*$", ln, re.IGNORECASE):
            out.append("\n")
            out.append("       * --- Auto-generated DB2 mock files ---\n")
            out.append("       SELECT DB2-IN-FILE.\n")  # harmless comment marker; some shops like it
            out.append(in_fd)
            out.append("\n")
            out.append(out_fd)
            out.append("       * --- End auto-generated files ---\n\n")
            inserted = True

    if inserted:
        return out

    # If no FILE SECTION, inject after DATA DIVISION.
    out = []
    for ln in src_lines:
        out.append(ln)
        if not inserted and re.search(r"^\s*DATA\s+DIVISION\.\s*$", ln, re.IGNORECASE):
            out.append("       FILE SECTION.\n\n")
            out.append("       * --- Auto-generated DB2 mock files ---\n")
            out.append(in_fd)
            out.append("\n")
            out.append(out_fd)
            out.append("       * --- End auto-generated files ---\n\n")
            inserted = True
    return out


def ensure_sqlcode_defined(src_lines: List[str]) -> List[str]:
    """
    If SQLCA include removed, we define SQLCODE in WORKING-STORAGE.
    Inject after WORKING-STORAGE SECTION line.
    """
    text = "".join(src_lines).upper()
    if "01  SQLCODE" in text or "77  SQLCODE" in text:
        return src_lines

    out = []
    inserted = False
    for ln in src_lines:
        out.append(ln)
        if not inserted and re.search(r"^\s*WORKING-STORAGE\s+SECTION\.\s*$", ln, re.IGNORECASE):
            out.append("       01  SQLCODE PIC S9(9) COMP VALUE 0.\n")
            out.append("       01  WS-EOF  PIC X VALUE 'N'.\n")
            out.append("           88  EOF VALUE 'Y'.\n")
            inserted = True
    return out


def gen_json_emitter_paragraph(select_vars: List[str], extra_vars: List[str]) -> str:
    """
    Emits one JSON object per processed record, as JSON Lines.
    We emit select vars + extra vars (like WS-STATUS, WS-SEGMENT).
    """
    fields = []
    for v in select_vars + extra_vars:
        # JSON key = lower snake-ish
        key = v.lower().replace("-", "_")
        fields.append((key, v))

    # COBOL DISPLAY builds a JSON string; keep simple; no escaping.
    lines = []
    lines.append("       JSON-EMIT.\n")
    lines.append("           DISPLAY '{' WITH NO ADVANCING.\n")
    for idx, (key, var) in enumerate(fields):
        comma = "," if idx < len(fields) - 1 else ""
        # Treat everything as string for portability; you can improve numeric detection later.
        lines.append(f"           DISPLAY '\"{key}\":\"' WITH NO ADVANCING.\n")
        lines.append(f"           DISPLAY {var} WITH NO ADVANCING.\n")
        lines.append(f"           DISPLAY '\"{comma}' WITH NO ADVANCING.\n")
    lines.append("           DISPLAY '}' .\n")
    lines.append("           EXIT.\n\n")
    return "".join(lines)


def gen_db2_stub_paragraphs(select_vars: List[str], in_move: str,
                           out_vars: List[str], out_move: str) -> str:
    lines = []
    lines.append("       * --- Auto-generated DB2 stub paragraphs ---\n")
    lines.append("       DB2-OPEN.\n")
    lines.append("           OPEN INPUT DB2-IN-FILE.\n")
    lines.append("           OPEN OUTPUT DB2-OUT-FILE.\n")
    lines.append("           EXIT.\n\n")

    lines.append("       DB2-FETCH.\n")
    lines.append("           READ DB2-IN-FILE INTO DB2-IN-REC\n")
    lines.append("               AT END\n")
    lines.append("                   MOVE 100 TO SQLCODE\n")
    lines.append("                   SET EOF TO TRUE\n")
    lines.append("               NOT AT END\n")
    lines.append("                   MOVE 0 TO SQLCODE\n")
    lines.append(in_move)
    lines.append("           END-READ.\n")
    lines.append("           EXIT.\n\n")

    lines.append("       DB2-UPDATE.\n")
    lines.append(out_move)
    lines.append("           WRITE DB2-OUT-REC.\n")
    lines.append("           EXIT.\n\n")

    lines.append("       DB2-CLOSE.\n")
    lines.append("           CLOSE DB2-IN-FILE.\n")
    lines.append("           CLOSE DB2-OUT-FILE.\n")
    lines.append("           EXIT.\n\n")
    lines.append("       * --- End auto-generated DB2 stubs ---\n\n")
    return "".join(lines)


def transform_exec_sql_blocks(lines: List[str], sql: SqlModel) -> List[str]:
    """
    Replaces EXEC SQL blocks with PERFORM DB2-*.
    Also captures:
      - cursor name (DECLARE)
      - FETCH host vars order
      - UPDATE set/where host vars
    """
    out = []
    in_sql = False
    sqlbuf: List[str] = []

    def flush_sql_block(block: str):
        blk = block.strip()
        upper = blk.upper()

        # DECLARE cursor: just ignore block but capture name
        m = DECLARE_CURSOR_RE.search(upper)
        if m:
            sql.cursor_name = m.group(1).upper()
            return ["           CONTINUE.\n"]  # placeholder

        # OPEN
        if OPEN_CURSOR_RE.search(block):
            return ["           PERFORM DB2-OPEN.\n"]

        # CLOSE
        if CLOSE_CURSOR_RE.search(block):
            return ["           PERFORM DB2-CLOSE.\n"]

        # FETCH
        mf = FETCH_RE.search(upper)
        if mf:
            vars_ = extract_host_vars(upper)
            if vars_:
                sql.select_into_vars = vars_
            return ["           PERFORM DB2-FETCH.\n"]

        # UPDATE
        mu = UPDATE_RE.search(upper)
        if mu:
            set_part = mu.group(2)
            where_part = mu.group(3)
            sql.update_set_vars = extract_host_vars(set_part)
            sql.update_where_vars = extract_host_vars(where_part)
            return ["           PERFORM DB2-UPDATE.\n", "           PERFORM JSON-EMIT.\n"]

        # INCLUDE SQLCA is removed upstream
        if INCLUDE_SQLCA_RE.search(upper):
            return []

        # Default: drop
        return ["           CONTINUE.\n"]

    for ln in lines:
        if not in_sql and EXEC_SQL_START_RE.search(ln):
            in_sql = True
            sqlbuf = [ln]
            # If single-line EXEC SQL ... END-EXEC.
            if END_EXEC_RE.search(ln):
                in_sql = False
                out.extend(flush_sql_block("".join(sqlbuf)))
            continue

        if in_sql:
            sqlbuf.append(ln)
            if END_EXEC_RE.search(ln):
                in_sql = False
                out.extend(flush_sql_block("".join(sqlbuf)))
            continue

        # Replace CALL 'CBLTDLI'
        if CALL_CBLTDLI_RE.search(ln):
            ln = CALL_CBLTDLI_RE.sub("CALL 'DLI-STUB'", ln)

        # Replace COMP-3 in PIC clauses
        if "COMP-3" in ln.upper():
            ln = COMP3_RE.sub("DISPLAY", ln)

        out.append(ln)

    return out


def remove_sqlca_include(lines: List[str]) -> List[str]:
    out = []
    in_inline_sqlca = False
    for ln in lines:
        up = ln.upper()
        if INCLUDE_SQLCA_RE.search(up):
            # might be "EXEC SQL INCLUDE SQLCA END-EXEC."
            if "END-EXEC" in up:
                continue
            in_inline_sqlca = True
            continue
        if in_inline_sqlca:
            if "END-EXEC" in up:
                in_inline_sqlca = False
            continue
        out.append(ln)
    return out


def append_generated_paragraphs(lines: List[str], generated: str) -> List[str]:
    # Append near end; safest to append just before last "END PROGRAM." if present.
    out = []
    inserted = False
    for ln in lines:
        if not inserted and re.search(r"^\s*END\s+PROGRAM\b", ln, re.IGNORECASE):
            out.append("\n")
            out.append(generated)
            inserted = True
        out.append(ln)
    if not inserted:
        out.append("\n")
        out.append(generated)
    return out


def write_python_harness(out_dir: Path, exe_path: str):
    harness = f"""#!/usr/bin/env python3
import json
import subprocess
import sys
from pathlib import Path

def run(exe: str, in_file: str, out_file: str):
    env = dict(**{{k:v for k,v in dict().items()}})
    # The stubbed COBOL uses fixed filenames db2_in.dat / db2_out.dat in cwd.
    # We copy/point them here for a predictable interface.
    Path("db2_in.dat").write_text(Path(in_file).read_text(encoding="utf-8"), encoding="utf-8")

    p = subprocess.run([exe], capture_output=True, text=True)
    # Program writes db2_out.dat + emits JSON Lines to stdout
    if Path("db2_out.dat").exists():
        Path(out_file).write_text(Path("db2_out.dat").read_text(encoding="utf-8"), encoding="utf-8")

    return {{
        "returncode": p.returncode,
        "stdout": p.stdout,
        "stderr": p.stderr,
        "jsonl": [json.loads(line) for line in p.stdout.splitlines() if line.strip().startswith("{{")],
    }}

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: run_cobol_case.py <exe> <db2_in.dat> <db2_out.dat>")
        sys.exit(2)
    result = run(sys.argv[1], sys.argv[2], sys.argv[3])
    print(json.dumps(result, indent=2))
"""
    path = out_dir / "run_cobol_case.py"
    path.write_text(harness, encoding="utf-8")
    path.chmod(0o755)


def preprocess(input_file: Path, output_file: Path, out_dir: Path):
    src_lines = input_file.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)

    # 1) Remove SQLCA include, replace COMP-3 early
    src_lines = remove_sqlca_include(src_lines)

    # 2) Parse variable defs from current source (post COMP-3->DISPLAY)
    var_defs = parse_working_storage_vars([COMP3_RE.sub("DISPLAY", l) for l in src_lines])

    # 3) Transform EXEC SQL blocks into PERFORM DB2-*
    sql = SqlModel()
    transformed = transform_exec_sql_blocks(src_lines, sql)

    # Need select vars to build input record; if none found, keep empty
    select_vars = sql.select_into_vars or []

    # OUT record vars: usually key + set vars; include select vars too for traceability
    out_vars = []
    for v in (select_vars + sql.update_set_vars + sql.update_where_vars):
        if v not in out_vars:
            out_vars.append(v)

    # If update vars weren’t found, still produce some output based on select vars
    if not out_vars:
        out_vars = select_vars[:] if select_vars else ["WS-STATUS", "WS-SEGMENT"]

    # 4) Generate file defs and stub paragraphs
    in_fd, in_move = gen_db2_file_defs(select_vars, var_defs)
    out_fd, out_move = gen_db2_out_defs(out_vars, var_defs)

    # 5) Inject ENV and FILE SECTION
    transformed = inject_env_division_for_files(transformed)
    transformed = ensure_sqlcode_defined(transformed)

    # Insert file section content
    transformed = inject_file_section(transformed, in_fd, out_fd)

    # 6) Add JSON emitter and DB2 stubs paragraphs
    # Extra vars to emit commonly: WS-STATUS, WS-SEGMENT if present
    extra_emit = []
    for candidate in ["WS-STATUS", "WS-SEGMENT"]:
        if candidate in var_defs or any(candidate in l.upper() for l in transformed):
            extra_emit.append(candidate)

    json_emit = gen_json_emitter_paragraph(select_vars, extra_emit)
    db2_stubs = gen_db2_stub_paragraphs(select_vars, in_move, out_vars, out_move)
    generated = json_emit + db2_stubs

    transformed = append_generated_paragraphs(transformed, generated)

    output_file.write_text("".join(transformed), encoding="utf-8")

    # 7) Generate Python harness wrapper
    write_python_harness(out_dir, exe_path=str(out_dir / "custproc"))

    print(f"Preprocessed: {input_file} -> {output_file}")
    print(f"Detected SELECT INTO vars: {select_vars}")
    print(f"Detected UPDATE vars (SET): {sql.update_set_vars}, (WHERE): {sql.update_where_vars}")
    print(f"Generated harness: {out_dir / 'run_cobol_case.py'}")


def main():
    if len(sys.argv) != 4:
        print("Usage: preprocessor.py <input.cbl> <output.cbl> <out_dir>")
        sys.exit(2)
    inp = Path(sys.argv[1])
    outp = Path(sys.argv[2])
    out_dir = Path(sys.argv[3])
    out_dir.mkdir(parents=True, exist_ok=True)
    preprocess(inp, outp, out_dir)


if __name__ == "__main__":
    main()
