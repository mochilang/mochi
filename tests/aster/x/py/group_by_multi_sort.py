# Code generated by Mochi transpiler.
# Version 0.10.36, generated on 2025-07-22 17:46 +0700
from __future__ import annotations
from dataclasses import dataclass
from typing import List, Dict
import dataclasses
@dataclass
class Item:
    a: str
    b: int
    val: int
items = [Item("x", 1, 2), Item("x", 2, 3), Item("y", 1, 4), Item("y", 2, 1)]
@dataclass
class GKey:
    a: str
    b: int
@dataclass
class GroupedGroup:
    key: GKey
    items: list
_grouped_groups = {}
for i in items:
    _g = _grouped_groups.setdefault(tuple([i.a, i.b]), GroupedGroup(GKey(i.a, i.b), []))
    _g.items.append(i)
@dataclass
class Grouped:
    a: str
    b: int
    total: any
grouped = [Grouped(g.key.a, g.key.b, sum([x.val for x in g.items])) for g in sorted(_grouped_groups.values(), key=lambda g: sum([x.val for x in g.items]), reverse=True)]
print([dataclasses.asdict(_x) for _x in grouped])
