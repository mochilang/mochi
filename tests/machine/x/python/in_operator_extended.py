# Generated by Mochi compiler v0.10.28 on 2025-07-18T04:04:03Z
from __future__ import annotations
import dataclasses


@dataclasses.dataclass
class Auto1:
    a: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


xs = [1, 2, 3]
ys = [x for x in xs if x % 2 == 1]
print(1 in ys)
print(2 in ys)
m = Auto1(a=1)
print("a" in m)
print("b" in m)
s = "hello"
print("ell" in s)
print("foo" in s)
