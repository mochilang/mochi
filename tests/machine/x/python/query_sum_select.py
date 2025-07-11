# Generated by Mochi Python compiler
from __future__ import annotations
from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")


def _sum(v):
    if hasattr(v, "Items"):
        v = v.Items
    if not isinstance(v, list):
        raise Exception("sum() expects list or group")
    s = 0.0
    for it in v:
        if it is None:
            continue
        if isinstance(it, (int, float)):
            s += float(it)
        else:
            raise Exception("sum() expects numbers")
    return s


nums = [1, 2, 3]
result = sum([n for n in nums if n > 1])
print(result)
