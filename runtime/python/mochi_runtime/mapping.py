"""Mochi runtime map helpers.

Mochi `keys(m)` and `values(m)` are spec'd to return key-sorted
sequences (matching vm3's sort-on-iteration behaviour) so AOT output
stays byte-equal to the oracle. Python `dict` preserves insertion
order, not key order, so these helpers wrap the standard accessors
with `sorted(...)`.
"""

from __future__ import annotations

from typing import TypeVar

K = TypeVar("K")
V = TypeVar("V")


def keys_sorted(m: dict[K, V]) -> list[K]:
    return sorted(m.keys())


def values_sorted(m: dict[K, V]) -> list[V]:
    return [m[k] for k in sorted(m.keys())]
