"""Mochi runtime float helpers.

`fdiv` implements IEEE 754 binary64 division for floats, returning
+Inf / -Inf / NaN on the zero-divisor cases instead of raising
ZeroDivisionError (which is the CPython default).
"""

from __future__ import annotations


def fdiv(a: float, b: float) -> float:
    if b == 0.0:
        if a == 0.0:
            return float("nan")
        return float("inf") if a > 0.0 else float("-inf")
    return a / b
