#!/usr/bin/env python3
"""Static touchpoint check for the Haxe SA target (no OCaml needed).

Usage: python3 tools/check_sa_target.py
Exit 0 = all touchpoints present, 1 = missing items listed.
"""
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

CHECKS = [
    ("src/core/globals.ml", ["| Sa", '| Sa -> "sa"', '| "sa" -> Sa']),
    ("src/context/common.ml", ['| Sa -> "sa"', "| Sa ->"]),
    ("src/compiler/compiler.ml", ['| Sa ->', 'add_std "sa"']),
    ("src/compiler/args.ml", ["--sa", "SetPlatform (Sa, file)", '| Sa -> "sa"']),
    ("src/compiler/generate.ml", ["Gensa.generate"]),
    ("src/macro/macroApi.ml", ["| Sa -> 12"]),
    ("src/optimization/analyzerTexpr.ml", ["| Lua | Python | Sa ->"]),
    ("src/generators/gensa.ml", [
        '@import \\"sa_std/io/print.sai\\"',
        "@main() -> i32:",
        "L_ENTRY:",
        "call @sa_print_bytes(&HAXE_SA_NOTE, 22)",
        "return 0",
    ]),
    ("std/sa/Boot.hx", ["package sa;", "class Boot"]),
    ("SA_TARGET.md", ["sa_plugin_ts", "sci/sa_std"]),
]

missing = []
for rel, needles in CHECKS:
    p = ROOT / rel
    if not p.exists():
        missing.append(f"MISSING FILE: {rel}")
        continue
    text = p.read_text(encoding="utf-8", errors="replace")
    for n in needles:
        if n not in text:
            missing.append(f"{rel}: needle not found: {n!r}")

if missing:
    print(f"FAIL ({len(missing)}):")
    for m in missing:
        print("  -", m)
    sys.exit(1)
print(f"OK: all {sum(len(n) for _, n in CHECKS)} touchpoints present.")
