#!/usr/bin/env python3
import difflib
import glob
import os
import subprocess

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
PY_DIR = os.path.join(ROOT, "tests", "compiler", "py")
CONVERTER = os.path.join(ROOT, "tools", "any2mochi", "py", "py2mochi.py")

for py_file in sorted(glob.glob(os.path.join(PY_DIR, "*.py.out"))):
    base = py_file[:-7]
    mochi_file = base + ".mochi"
    out_file = base + ".mochi.out"
    err_file = base + ".error"

    try:
        code = subprocess.check_output(
            ["python3", CONVERTER, py_file], text=True, stderr=subprocess.STDOUT
        )
    except subprocess.CalledProcessError as e:
        with open(err_file, "w") as f:
            f.write("py2mochi failed:\n")
            f.write(e.output)
        if os.path.exists(out_file):
            os.remove(out_file)
        continue


    expected = open(mochi_file, encoding="utf-8").read()

    def strip_comments(s: str) -> str:
        return "\n".join([ln.split("//")[0].rstrip() for ln in s.splitlines()])

    if "".join(strip_comments(code).split()) == "".join(
        strip_comments(expected).split()
    ):
        with open(out_file, "w", encoding="utf-8") as f:
            f.write(code)
        if os.path.exists(err_file):
            os.remove(err_file)
    else:
        diff = "\n".join(
            difflib.unified_diff(
                expected.splitlines(),
                code.splitlines(),
                fromfile="expected",
                tofile="generated",
                lineterm="",
                n=3,
            )
        )
        with open(err_file, "w", encoding="utf-8") as f:
            f.write("generated code does not match expected\n")
            f.write(diff)
        if os.path.exists(out_file):
            os.remove(out_file)
