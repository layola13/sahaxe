#!/usr/bin/env python3
"""Structural check for Haxe-SA emitted .sa files (no OCaml/sa needed).

Rules (from sala 02_sa_syntax + 06_limitations):
- labels `L_X:` start at column 0; instructions are indented
- every label block ends with jmp / br / return / panic
- no instruction follows a terminator within the same block
- @const / @import precede @main

Usage: python3 tools/check_sa_shape.py <file.sa> [...]
"""
import sys

TERMINATORS = ("jmp ", "br ", "return", "panic(")


def check(path):
    errs = []
    lines = open(path, encoding="utf-8").read().splitlines()
    seen_main = False
    pending = None  # (label, lineno, has_instr, terminated)
    def flush():
        nonlocal pending
        if pending and not pending[3]:
            errs.append(f"{path}:{pending[1]}: block {pending[0]} has no terminator")
        pending = None
    for i, raw in enumerate(lines, 1):
        s = raw.strip()
        if not s or s.startswith("//"):
            continue
        if s.startswith("@import") or s.startswith("@const"):
            if seen_main:
                errs.append(f"{path}:{i}: toplevel decl after @main")
            continue
        if s.startswith("@main"):
            seen_main = True
            flush()
            continue
        if s.startswith("@") or s.startswith("#"):
            flush()
            continue
        if s.endswith(":") and " " not in s and raw[:1] not in (" ", "\t"):
            flush()
            pending = (s[:-1], i, False, False)
            continue
        if pending is None:
            if seen_main:
                errs.append(f"{path}:{i}: instruction outside block: {s[:40]}")
            continue
        if pending[3]:
            errs.append(f"{path}:{i}: unreachable after terminator: {s[:40]}")
        term = s.startswith(TERMINATORS)
        pending = (pending[0], pending[1], True, term)
    flush()
    return errs


def main():
    all_errs = []
    for f in sys.argv[1:]:
        all_errs.extend(check(f))
    if all_errs:
        print(f"FAIL ({len(all_errs)}):")
        for e in all_errs:
            print("  -", e)
        return 1
    print(f"OK: {len(sys.argv) - 1} file(s) structurally sound.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
