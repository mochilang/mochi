"""Mochi runtime query helpers.

The Query DSL lowerer cannot emit a bare `sorted(...)` call because
Mochi `let` bindings can shadow Python builtins (e.g. the parser
accepts `let sorted = from n in nums order by n select n`). The
shadowing makes the inner `sorted` reference fail with
UnboundLocalError. Routing the call through a runtime helper keeps
the builtin reference qualified.
"""

from __future__ import annotations

from typing import TypeVar

T = TypeVar("T")


def sort_asc(xs: list[T]) -> list[T]:
    return sorted(xs)


def sum_i64(xs: list[int]) -> int:
    return sum(xs)


def sum_f64(xs: list[float]) -> float:
    return sum(xs, 0.0)
