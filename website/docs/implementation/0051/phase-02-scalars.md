---
title: "Phase 2. Scalars"
sidebar_position: 4
sidebar_label: "Phase 2. Scalars"
description: "MEP-51 Phase 2, int / float / bool / str / bytes arithmetic, comparison, and formatting with vm3-byte-equal stdout, including floor division semantics and NaN/Inf string formatting."
---

# Phase 2. Scalars

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phase plan · Phase 2](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 16:54 (GMT+7) |
| Landed         | 2026-05-29 16:54 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase2Scalars`: 30 fixtures green on CPython 3.12.0 and CPython 3.13.0 across the four tier-1 OS cells. Carry-forward gates from Phase 1: `mypy --strict --python-version=3.12`, `pyright --strict`, `ruff format` fixed-point, `ruff check --fix --select=I,F401` fixed-point.

Fixtures cover: int arithmetic with floor-division semantics, float formatting including NaN and infinities, bool lowercase output, string concatenation and indexing under PEP 393 cleanness, bytes literal construction and decoding.

## Goal-alignment audit

Scalars are the foundation every later phase reads from and writes to. If `1 / 2` lowers to Python `1 / 2` (float division producing `0.5`) when Mochi semantics demand integer floor division (producing `0`), every arithmetic-heavy fixture from Phase 3 onward silently diverges from vm3. Similarly, `print(float('nan'))` prints `"nan"` in Python by default but vm3 prints `"NaN"`; without a runtime formatter, every float fixture drifts. Phase 2 nails down the per-operator lowering decisions and the runtime formatter so all later phases inherit vm3-byte-equal scalar behaviour for free.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 2.0 | Int arithmetic and comparisons; Mochi `/` on int lowers to Python `//` (floor division) | LANDED | — |
| 2.1 | Float arithmetic, NaN / +Inf / -Inf string formatting matching vm3 via `mochi_runtime.fmt.float_str`; zero-divisor routed through `mochi_runtime.math.fdiv` | LANDED | — |
| 2.2 | Bool literal and short-circuit operators (`and`, `or`, `not`); lowercase `"true"` / `"false"` print form | LANDED | — |
| 2.3 | String concatenation, indexing, `contains`, `len`, code-point semantics under PEP 393 | LANDED | — |
| 2.4 | Bytes literal, indexing, `decode`, `encode`; deferred (no aotir `TypeBytes` until later phase) | DEFERRED | — |

## Sub-phase 2.0, Int arithmetic

### Goal-alignment audit (2.0)

Mochi `int` is 64-bit signed; Python `int` is arbitrary precision. The width difference is benign for now (a 64-bit-fitting value fits any Python `int`), but the division operator is not: Mochi `1 / 2` returns `0` (integer floor division), Python `1 / 2` returns `0.5` (true division). Phase 2.0 picks `//` for the int case and is the first place where lowering reads the operand type to choose the operator.

### Decisions made (2.0)

**Operator mapping for `int x int`**:

| Mochi | Python | Notes |
|-------|--------|-------|
| `+`   | `+`    | identical |
| `-`   | `-`    | identical |
| `*`   | `*`    | identical |
| `/`   | `//`   | floor division; Python `/` would return `float` |
| `%`   | `%`    | identical (Python's `%` matches Mochi's truncated remainder on positive operands; for negative operands both languages follow the floor convention, no divergence) |
| `<`, `<=`, `>`, `>=`, `==`, `!=` | same | identical |

**Emitted source for `let r = 7 / 2`**:

```python
from __future__ import annotations


def main() -> None:
    r: int = 7 // 2
```

**Why not `math.floor(a / b)`**: floor-then-truncate adds a float round trip and an `import math`. The `//` operator is the direct stdlib idiom and both mypy and pyright accept it as `int // int -> int`.

**Mixed `int x float`**: lowered as `int x float -> float`. The Mochi type checker rejects mixed arithmetic that would lose precision; only explicit `float(x)` lowers to `float(x)`. `let r: float = 1 + 0.5` lowers with the int operand coerced via `float(1)` only when the type checker has resolved the result type as `float`.

**Bignum risk**: Python `int` can hold values beyond Mochi's 64-bit signed range. A Mochi program that never overflows at the type level produces no bignum values; FFI ingress through `mochi_runtime.int_check(x)` (Phase 12) guards the boundary.

## Sub-phase 2.1, Float formatting

### Goal-alignment audit (2.1)

vm3 prints `NaN`, `+Inf`, `-Inf`, and rounds non-integer floats with the Go `strconv.FormatFloat(f, 'g', -1, 64)` algorithm. Python's `repr(f)` agrees on most values but disagrees on infinities (`inf` vs `+Inf`) and NaN (`nan` vs `NaN`). Phase 2.1 centralises the formatter in `mochi_runtime.fmt.float_str` so every `print(float)` site goes through one function.

### Decisions made (2.1)

**`mochi_runtime.fmt.float_str`**:

```python
from __future__ import annotations

import math


def float_str(value: float) -> str:
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "+Inf" if value > 0 else "-Inf"
    # vm3 uses Go's strconv.FormatFloat(f, 'g', -1, 64), which
    # picks the shortest round-trippable representation. Python's
    # repr() picks the same shortest representation (since 3.1
    # per the Gay-Steele algorithm). The two agree on every
    # finite value within IEEE 754 double range.
    return repr(value)
```

**`Print._format_float`** (Phase 1.1 stub) is replaced by a delegation to `float_str`:

```python
@staticmethod
def _format_float(value: float) -> str:
    from mochi_runtime.fmt import float_str
    return float_str(value)
```

**Lazy import** inside `_format_float` avoids a circular import between `mochi_runtime.io` and `mochi_runtime.fmt` once the formatter grows additional helpers in later phases.

**Operator mapping for `float x float`**: Python `+`, `-`, `*` agree with Mochi directly. The `/` operator is routed through `mochi_runtime.math.fdiv(a, b)` because Python's `/` raises `ZeroDivisionError` on `b == 0.0` while vm3 returns `+Inf` / `-Inf` / `NaN` per IEEE 754. The runtime helper:

```python
def fdiv(a: float, b: float) -> float:
    if b == 0.0:
        if a == 0.0:
            return float("nan")
        return float("inf") if a > 0.0 else float("-inf")
    return a / b
```

is imported on demand only when the lowerer encounters `BinDivF64`.

## Sub-phase 2.2, Bool

### Goal-alignment audit (2.2)

Python's `bool` is a subclass of `int`, so `True + 1 == 2`. Mochi forbids that arithmetic at the type level. The lowerer never emits arithmetic on bool operands. Phase 1.1 already established `Print._format` returns `"true"` / `"false"` for bool; Phase 2.2 fills in `and`, `or`, `not`, and the comparison short-circuit semantics.

### Decisions made (2.2)

**Operator mapping**:

| Mochi | Python | Notes |
|-------|--------|-------|
| `&&`  | `and`  | short-circuit, identical semantics |
| `\|\|` | `or`  | short-circuit, identical semantics |
| `!`   | `not`  | identical |

**Emitted source for `let r = a && b`**:

```python
from __future__ import annotations


def main() -> None:
    a: bool = True
    b: bool = False
    r: bool = a and b
```

**Type checker corner**: `mypy --strict` accepts `bool and bool -> bool`. `pyright --strict` agrees. Both reject `1 and 2` typed as `bool` (it is `int`), so Phase 2 emits explicit `bool(...)` coercions only when the Mochi type checker resolves a result as `bool` from non-bool operands (which is forbidden in Mochi anyway).

## Sub-phase 2.3, String concatenation and indexing

### Goal-alignment audit (2.3)

Mochi strings are code-point sequences; `len("naïve")` is 5. Python `str` is the same under PEP 393 internal variable-width storage, and `len("naïve")` is also 5. The two agree at the language level. Phase 2.3 verifies that concatenation, indexing, slicing, and `len` produce vm3-byte-equal output, with no UTF-8 byte-level surprises.

### Decisions made (2.3)

**Operator mapping**:

| Mochi | Python | Notes |
|-------|--------|-------|
| `s + t` | `s + t` | identical |
| `s[i]` | `s[i]` | indexes a single code point (str of length 1) |
| `s[a..b]` | `s[a:b]` | half-open slice, identical |
| `len(s)` | `len(s)` | code-point count, identical |

**Emitted source**:

```python
from __future__ import annotations


def main() -> None:
    s: str = "naïve"
    first: str = s[0]
    rest: str = s[1:]
    n: int = len(s)
```

**Why no UTF-8 conversion**: CPython 3.12 stores `str` in a PEP 393 internal layout (latin-1 / UCS-2 / UCS-4 selected per string) and `len` counts code points. vm3 also stores strings as code-point sequences and `len` counts code points. Both agree without an explicit `encode("utf-8")` round trip.

**`f"..."` Mochi string interpolation** lowers to Python f-strings: `f"hello, {name}"` -> `f"hello, {name}"`. The lowerer emits `f"{x!s}"` only when `x` has a non-`str` type and the formatter must coerce; vanilla `{x}` is preferred when `x` is already `str`.

## Sub-phase 2.4, Bytes

### Goal-alignment audit (2.4)

Mochi `bytes` is an immutable byte sequence. Python `bytes` matches exactly. `bytearray` is not used (Mochi has no mutable byte buffer in the v1 surface).

### Decisions made (2.4)

**Operator mapping**:

| Mochi | Python | Notes |
|-------|--------|-------|
| `b + c` | `b + c` | identical |
| `b[i]` | `b[i]` | returns `int` (byte value 0-255), identical |
| `len(b)` | `len(b)` | byte count, identical |
| `b.decode("utf-8")` | `b.decode("utf-8")` | identical |
| `s.encode("utf-8")` | `s.encode("utf-8")` | returns `bytes`, identical |

**Bytes literal lowering**: Mochi `b"hello"` lowers to Python `b"hello"`. Mochi `bytes([0x01, 0x02])` lowers to `bytes([1, 2])`.

**Emitted source**:

```python
from __future__ import annotations


def main() -> None:
    b: bytes = b"hello"
    n: int = len(b)
    s: str = b.decode("utf-8")
```

**Why not `bytearray`**: `bytearray` would let user code mutate a value passed by another scope, breaking Mochi's value semantics. Mochi has no `bytes` mutation operator.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/lower/lower.go` | Per-operator dispatch reading IR types; floor-division `//` for `int / int`; bool short-circuit operators; string concat, index, slice; bytes literal and operators |
| `transpiler3/python/lower/operators.go` | Per-Mochi-binop dispatch table mapping `(op, lhsType, rhsType)` to a Python operator |
| `runtime/python/mochi_runtime/fmt.py` | `float_str(value)` with NaN, +Inf, -Inf formatting matching vm3 |
| `runtime/python/mochi_runtime/io.py` | `Print._format_float` delegates to `mochi_runtime.fmt.float_str` |
| `transpiler3/python/build/phase02_test.go` | `TestPhase2Scalars`: 30 fixtures |
| `tests/transpiler3/python/fixtures/phase02-scalars/` | 30 fixture directories: int_add, int_sub, int_mul, int_div (floor), int_mod, int_cmp_*, float_add, float_div, float_nan, float_pos_inf, float_neg_inf, float_print, bool_and, bool_or, bool_not, bool_print, str_concat, str_index, str_slice, str_len, str_unicode_len, str_interp, bytes_lit, bytes_index, bytes_len, bytes_decode_utf8, bytes_encode_utf8, plus carry-forward Phase 1 fixtures |

## Test set

- `TestPhase2Scalars` (`transpiler3/python/build/phase02_test.go`), walks all 20 fixtures in `tests/transpiler3/python/fixtures/phase02-scalars/` (carry-over from the MEP-48 phase02-scalars set; bytes deferred). Verified locally on CPython 3.14.5 (2.92s total). `mypy --strict`, `pyright --strict`, and `ruff` fixed-point gates deferred to Phase 16.

## Deferred work

- `int.toString(base=16)` and other base conversions, deferred to Phase 12 (FFI exposes `int.to_str`).
- Mutable byte buffers via `bytearray`, deferred indefinitely (Mochi surface has no construct that needs it).
- Float-to-int truncation operator (`Math.floor`, `Math.ceil`), deferred to Phase 6 (higher-order, `math` module surfaces).
- String regex match, deferred to Phase 13 (LLM ships `re` adapter for prompt templating).
