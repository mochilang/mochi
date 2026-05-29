"""Mochi runtime formatters.

`float_str` is the canonical float-to-string routine used by Print
and any later phase that needs vm3-byte-equal float output.
"""

from __future__ import annotations

import math


def float_str(value: float) -> str:
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "+Inf" if value > 0 else "-Inf"
    if value == int(value) and not math.isinf(value):
        return repr(int(value))
    return repr(value)


def str_from(value: object) -> str:
    """Mochi `str(x)` lowering. Matches Print byte-for-byte: bool to
    lowercase `true`/`false`, float through `float_str`, otherwise
    Python's builtin `str` (which is byte-equal for int and string)."""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        return float_str(value)
    return str(value)
