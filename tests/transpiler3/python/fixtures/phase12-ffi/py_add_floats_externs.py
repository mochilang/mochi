"""Externs sidecar for the py_add_floats fixture (Phase 12.0).

Each `extern python fun X(...)` in the Mochi source resolves to a same-
named callable imported from this module. The generated user code does
`from mochi_user_py_add_floats_externs import py_add` and calls
`py_add(1.5, 2.5)` directly.
"""

from __future__ import annotations


def py_add(x: float, y: float) -> float:
    return x + y
