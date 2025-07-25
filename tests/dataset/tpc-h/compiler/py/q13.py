# Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    c_count: int
    custdist: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto2:
    c_count: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Customer:
    c_custkey: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Order:
    o_orderkey: int
    o_custkey: int
    o_comment: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")
UNDEFINED = object()


def _count(v):
    if isinstance(v, list):
        return len(v)
    if hasattr(v, "Items"):
        return len(v.Items)
    raise Exception("count() expects list or group")


class _Group(Generic[K, T]):

    def __init__(self, key: K):
        self.key = key
        self.Items: list[T] = []
        self.items = self.Items

    def __iter__(self):
        return iter(self.Items)

    def __len__(self):
        return len(self.Items)


def _group_by(src: list[T], keyfn: Callable[[T], K]) -> list[_Group[K, T]]:
    groups: dict[str, _Group[K, T]] = {}
    order: list[str] = []
    for it in src:
        if isinstance(it, (list, tuple)):
            key = keyfn(*it)
        else:
            key = keyfn(it)
        if isinstance(key, dict):
            import types

            key = types.SimpleNamespace(**key)
        ks = str(key)
        g = groups.get(ks)
        if not g:
            g = _Group(key)
            groups[ks] = g
            order.append(ks)
        g.Items.append(it)
    return [groups[k] for k in order]


def _query(src, joins, opts):
    items = [[v] for v in src]
    for j in joins:
        joined = []
        if j.get("right") and j.get("left"):
            matched = [False] * len(j["items"])
            for left in items:
                m = False
                for ri, right in enumerate(j["items"]):
                    keep = True
                    if j.get("on"):
                        keep = j["on"](*left, right)
                    if not keep:
                        continue
                    m = True
                    matched[ri] = True
                    joined.append(left + [right])
                if not m:
                    joined.append(left + [None])
            for ri, right in enumerate(j["items"]):
                if not matched[ri]:
                    undef = [None] * (len(items[0]) if items else 0)
                    joined.append(undef + [right])
        elif j.get("right"):
            for right in j["items"]:
                m = False
                for left in items:
                    keep = True
                    if j.get("on"):
                        keep = j["on"](*left, right)
                    if not keep:
                        continue
                    m = True
                    joined.append(left + [right])
                if not m:
                    undef = [None] * (len(items[0]) if items else 0)
                    joined.append(undef + [right])
        else:
            for left in items:
                m = False
                for right in j["items"]:
                    keep = True
                    if j.get("on"):
                        keep = j["on"](*left, right)
                    if not keep:
                        continue
                    m = True
                    joined.append(left + [right])
                if j.get("left") and (not m):
                    joined.append(left + [None])
        items = joined
    if opts.get("where"):
        items = [r for r in items if opts["where"](*r)]
    if opts.get("sortKey"):

        def _key(it):
            k = opts["sortKey"](*it)
            if isinstance(k, (list, tuple, dict)):
                return str(k)
            return k

        items.sort(key=_key)
    if "skip" in opts:
        n = opts["skip"]
        if n < 0:
            n = 0
        items = items[n:] if n < len(items) else []
    if "take" in opts:
        n = opts["take"]
        if n < 0:
            n = 0
        items = items[:n] if n < len(items) else items
    res = []
    for r in items:
        res.append(opts["select"](*r))
    return res


def test_Q13_groups_customers_by_non_special_order_count():
    assert grouped == [Auto1(c_count=2, custdist=1), Auto1(c_count=0, custdist=2)]


customer = [Customer(c_custkey=1), Customer(c_custkey=2), Customer(c_custkey=3)]
orders = [
    Order(o_orderkey=100, o_custkey=1, o_comment="fast delivery"),
    Order(o_orderkey=101, o_custkey=1, o_comment="no comment"),
    Order(o_orderkey=102, o_custkey=2, o_comment="special requests only"),
]
per_customer = [
    Auto2(
        c_count=len(
            [
                o
                for o in orders
                if (o.o_custkey == c.c_custkey and (not "special" in o.o_comment))
                and (not "requests" in o.o_comment)
            ]
        )
    )
    for c in customer
]


def _q0():
    _src = per_customer
    _rows = _query(_src, [], {"select": lambda x: x})
    _groups = _group_by(_rows, lambda x: x.c_count)
    _items1 = _groups
    _items1 = sorted(_items1, key=lambda g: -g.key)
    return [Auto1(c_count=g.key, custdist=len(g)) for g in _items1]


grouped = _q0()
print(json.dumps(grouped, separators=(",", ":"), default=lambda o: vars(o)))
test_Q13_groups_customers_by_non_special_order_count()
