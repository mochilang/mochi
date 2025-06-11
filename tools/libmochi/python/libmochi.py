import json
import os
import subprocess
import tempfile
from collections.abc import Sequence
from typing import Any

__all__ = ["run", "call"]


def _run(code: str, mochi_bin: str = "mochi") -> str:
    """Execute Mochi source code and return stdout as a string."""
    with tempfile.NamedTemporaryFile("w", suffix=".mochi", delete=False) as f:
        f.write(code)
        fname = f.name
    try:
        proc = subprocess.run([mochi_bin, "run", fname], capture_output=True, text=True)
        if proc.returncode != 0:
            raise RuntimeError(f"mochi exited with status {proc.returncode}: {proc.stderr}")
        return proc.stdout
    finally:
        os.unlink(fname)


def run(code: str, mochi_bin: str = "mochi") -> str:
    """Execute Mochi source code and return its standard output."""
    return _run(code, mochi_bin)


def call(code: str, func: str, *args: Any, mochi_bin: str = "mochi") -> Any:
    """Call ``func`` defined in ``code`` with ``args`` and return the result.

    ``code`` should contain the Mochi function definition. The result is
    obtained by wrapping the call with the ``json`` builtin and decoding the
    output.
    """
    args_literal = ", ".join(_to_mochi(a) for a in args)
    snippet = f"{code}\njson({func}({args_literal}))\n"
    out = _run(snippet, mochi_bin)
    return json.loads(out.strip())


def _to_mochi(v: Any) -> str:
    if v is None:
        return "null"
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, (int, float)):
        return str(v)
    if isinstance(v, str):
        return "\"" + v.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    if isinstance(v, Sequence) and not isinstance(v, (str, bytes, bytearray)):
        return "[" + ", ".join(_to_mochi(x) for x in v) + "]"
    if isinstance(v, dict):
        items = ", ".join(f'{_to_mochi(k)}: {_to_mochi(val)}' for k, val in v.items())
        return "{" + items + "}"
    raise TypeError(f"unsupported value type: {type(v).__name__}")
