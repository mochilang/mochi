from __future__ import annotations
from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")


def _get(obj, name):
    if obj is None:
        return None
    if isinstance(obj, dict):
        if name in obj:
            return obj[name]
    if hasattr(obj, name):
        return getattr(obj, name)
    if name == "items" and hasattr(obj, "Items"):
        return getattr(obj, "Items")
    if isinstance(obj, (list, tuple)):
        for it in obj:
            try:
                return _get(it, name)
            except Exception:
                pass
    raise Exception("field not found: " + name)


s = "catch"
print(str("cat" in s).lower())
print(str("dog" in s).lower())
