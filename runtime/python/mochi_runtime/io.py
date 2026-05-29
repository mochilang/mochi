"""Mochi runtime printer.

Provides Print.line, the single sink for `print(...)` in lowered Mochi
programs. Dispatch matches vm3 byte-for-byte: bools render as
lowercase `true`/`false`, ints render via str, strings render as-is,
floats fall through to repr (Phase 1; Phase 2.1 will canonicalise
NaN/Inf/round-trip formatting).
"""

from __future__ import annotations

import sys
from typing import Final


class Print:
    """Cross-target Mochi print sink.

    Class form (rather than module-level functions) matches the
    naming convention across MEP-47..MEP-51 (mochi.runtime.io.Print)
    and gives a single home for future overloads such as Print.error.
    """

    @staticmethod
    def line(value: object) -> None:
        sys.stdout.write(Print._format(value))
        sys.stdout.write("\n")

    @staticmethod
    def _format(value: object) -> str:
        if isinstance(value, bool):
            return "true" if value else "false"
        if isinstance(value, float):
            return Print._format_float(value)
        return str(value)

    @staticmethod
    def _format_float(value: float) -> str:
        from mochi_runtime.fmt import float_str

        return float_str(value)


_PRINT_LINE: Final = Print.line
