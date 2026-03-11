#!/usr/bin/env python3
"""
Single-test runner for dubsar.

Usage:
  run_test.py roundtrip <binary> <input.dub>
      Parses <input.dub>, re-parses the output, and checks the two outputs
      are identical (round-trip stability).

  run_test.py error <binary> <input.dub>
      Checks that parsing <input.dub> exits with a non-zero status code.
"""

import difflib
import subprocess
import sys
import tempfile
from pathlib import Path


def run(binary: str, path: str) -> tuple[int, str, str]:
    result = subprocess.run([binary, path], capture_output=True, text=True)
    return result.returncode, result.stdout, result.stderr


def roundtrip(binary: str, input_path: str) -> bool:
    rc, first_pass, err = run(binary, input_path)
    if rc != 0:
        print(f"FAIL: parse error on first pass:\n{err}", file=sys.stderr)
        return False

    with tempfile.NamedTemporaryFile(mode="w", suffix=".dub", delete=False) as tmp:
        tmp.write(first_pass)
        tmp_path = tmp.name

    try:
        rc2, second_pass, err2 = run(binary, tmp_path)
        if rc2 != 0:
            print(f"FAIL: parse error on second pass:\n{err2}", file=sys.stderr)
            return False

        if first_pass != second_pass:
            diff = "".join(
                difflib.unified_diff(
                    first_pass.splitlines(keepends=True),
                    second_pass.splitlines(keepends=True),
                    fromfile="first-pass",
                    tofile="second-pass",
                )
            )
            print(f"FAIL: round-trip output differs:\n{diff}", file=sys.stderr)
            return False
    finally:
        Path(tmp_path).unlink(missing_ok=True)

    return True


def error(binary: str, input_path: str) -> bool:
    rc, out, _err = run(binary, input_path)
    if rc == 0:
        print(
            f"FAIL: expected parse failure but binary exited 0.\nOutput:\n{out}",
            file=sys.stderr,
        )
        return False
    return True


def main() -> int:
    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <mode> <binary> <input.dub>", file=sys.stderr)
        return 1

    mode, binary, input_path = sys.argv[1], sys.argv[2], sys.argv[3]

    if mode == "roundtrip":
        ok = roundtrip(binary, input_path)
    elif mode == "error":
        ok = error(binary, input_path)
    else:
        print(f"Unknown mode: {mode!r}. Expected 'roundtrip' or 'error'.",
              file=sys.stderr)
        return 1

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
