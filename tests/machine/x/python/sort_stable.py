from __future__ import annotations
from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")


def _sort_key(k):
    if isinstance(k, (list, tuple, dict)):
        return str(k)
    return k


items = [{"n": 1, "v": "a"}, {"n": 1, "v": "b"}, {"n": 2, "v": "c"}]
result = [i["v"] for i in sorted([i for i in items], key=lambda i: _sort_key(i["n"]))]
print(result)
