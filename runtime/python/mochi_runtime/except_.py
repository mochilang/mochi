"""Mochi panic / try-catch runtime primitives for the Phase 11.0 surface.

Mochi's error model is a single integer code (MOCHI_ERR_*) plus a
human-readable message. User code raises via `panic(code, msg)` and
recovers via `try { ... } catch e { ... }` where `e` binds the integer
code in the catch body. Built-in runtime faults (division by zero,
index out of bounds) carry the same code surface so a generic catch
handler can recover from any failure.

Phase 11.0 maps the surface as follows:

  * `panic(code, msg)` lowers to `raise MochiPanic(code, msg)`.
  * `try { ... } catch e { ... }` lowers to a Python `try / except`
    block over `(MochiPanic, ZeroDivisionError, IndexError)`. The
    `_panic_code` helper converts the Python-native exceptions to the
    canonical Mochi code (5 for ZeroDivisionError, 4 for IndexError)
    while preserving MochiPanic.code verbatim. The catch body sees `e`
    bound to that int code, matching the C lowering semantics.

The choice of `Exception` as the base (not `BaseException`) means
KeyboardInterrupt and SystemExit pass through, which matches the
intent of the Mochi error model: panics are program-level faults,
not interpreter-level controls.

Async + MochiResult are deferred to Phase 11.1 (no v1 fixtures
exercise AsyncExpr / AwaitExpr / TypeFuture yet); when they land,
asyncio.Future wrappers will integrate via the same MochiPanic class
on the result side.
"""

from __future__ import annotations


class MochiPanic(Exception):
    """A Mochi panic carrying an integer error code and a message.

    Subclasses Exception (not BaseException) so KeyboardInterrupt and
    SystemExit are not intercepted by Mochi `catch` blocks. The code
    field matches the MOCHI_ERR_* constants:

      1 FETCH, 2 PARSE, 3 TYPE, 4 INDEX, 5 DIVZERO,
      6 OVERFLOW, 7 FFI, 8 LLM, 9 ASSERT
    """

    __slots__ = ("code", "msg")

    def __init__(self, code: int, msg: str) -> None:
        super().__init__(msg)
        self.code = code
        self.msg = msg


def _panic_code(exc: BaseException) -> int:
    """Return the canonical Mochi error code for a caught exception.

    Mochi catches collapse `MochiPanic`, `ZeroDivisionError`, and
    `IndexError` into a single integer surface. New built-in faults
    extend this mapping; user panics keep their own code untouched.
    """
    if isinstance(exc, MochiPanic):
        return exc.code
    if isinstance(exc, ZeroDivisionError):
        return 5
    if isinstance(exc, IndexError):
        return 4
    return 0
