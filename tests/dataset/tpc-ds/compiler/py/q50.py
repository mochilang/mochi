# Generated by Mochi compiler v0.10.27 on 2025-07-17T17:50:21Z
from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    s_store_name: str
    d30: int
    d31_60: int
    d61_90: int
    d91_120: int
    d_gt_120: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto2:
    s: Store
    diff: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class DateDim:
    d_date_sk: int
    d_year: int
    d_moy: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Store:
    s_store_sk: int
    s_store_name: str
    s_company_id: int
    s_street_number: str
    s_street_name: str
    s_street_type: str
    s_suite_number: str
    s_city: str
    s_county: str
    s_state: str
    s_zip: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class StoreReturn:
    ticket: int
    item: int
    returned: int
    customer: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class StoreSale:
    ticket: int
    item: int
    sold: int
    customer: int
    store: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")
UNDEFINED = object()


class _Group(Generic[K, T]):

    def __init__(self, key: K):
        self.key = key
        self.Items: list[T] = []
        self.items = self.Items

    def __iter__(self):
        return iter(self.Items)

    def __len__(self):
        return len(self.Items)


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


def test_TPCDS_Q50_simplified():
    assert result == [
        Auto1(s_store_name="Main", d30=1, d31_60=1, d61_90=1, d91_120=1, d_gt_120=1)
    ]


store_sales = [
    StoreSale(ticket=1, item=101, sold=1, customer=1, store=1),
    StoreSale(ticket=2, item=102, sold=1, customer=1, store=1),
    StoreSale(ticket=3, item=103, sold=1, customer=1, store=1),
    StoreSale(ticket=4, item=104, sold=1, customer=1, store=1),
    StoreSale(ticket=5, item=105, sold=1, customer=1, store=1),
]
store_returns = [
    StoreReturn(ticket=1, item=101, returned=16, customer=1),
    StoreReturn(ticket=2, item=102, returned=46, customer=1),
    StoreReturn(ticket=3, item=103, returned=76, customer=1),
    StoreReturn(ticket=4, item=104, returned=111, customer=1),
    StoreReturn(ticket=5, item=105, returned=151, customer=1),
]
date_dim = [
    DateDim(d_date_sk=1, d_year=2001, d_moy=7),
    DateDim(d_date_sk=16, d_year=2001, d_moy=8),
    DateDim(d_date_sk=46, d_year=2001, d_moy=8),
    DateDim(d_date_sk=76, d_year=2001, d_moy=8),
    DateDim(d_date_sk=111, d_year=2001, d_moy=8),
    DateDim(d_date_sk=151, d_year=2001, d_moy=8),
]
store = [
    Store(
        s_store_sk=1,
        s_store_name="Main",
        s_company_id=1,
        s_street_number="1",
        s_street_name="Main",
        s_street_type="St",
        s_suite_number="100",
        s_city="City",
        s_county="County",
        s_state="CA",
        s_zip="12345",
    )
]
year = 2001
month = 8
joined = _query(
    store_sales,
    [
        {
            "items": store_returns,
            "on": lambda ss, sr: (ss.ticket == sr.ticket and ss.item == sr.item)
            and ss.customer == sr.customer,
        },
        {"items": date_dim, "on": lambda ss, sr, d1: ss.sold == d1.d_date_sk},
        {
            "items": date_dim,
            "on": lambda ss, sr, d1, d2: (
                sr.returned == d2.d_date_sk and d2.d_year == year
            )
            and d2.d_moy == month,
        },
        {"items": store, "on": lambda ss, sr, d1, d2, s: ss.store == s.s_store_sk},
    ],
    {"select": lambda ss, sr, d1, d2, s: Auto2(s=s, diff=sr.returned - ss.sold)},
)


def _q0():
    _groups = {}
    _order = []
    for j in joined:
        _k = j.s
        _ks = str(_k)
        g = _groups.get(_ks)
        if not g:
            g = _Group(_k)
            _groups[_ks] = g
            _order.append(_ks)
        g.Items.append(j)
    _items1 = [_groups[k] for k in _order]
    return [
        Auto1(
            s_store_name=g.key.s_store_name,
            d30=len([1 for x in g if x.diff <= 30]),
            d31_60=len([1 for x in g if x.diff > 30 and x.diff <= 60]),
            d61_90=len([1 for x in g if x.diff > 60 and x.diff <= 90]),
            d91_120=len([1 for x in g if x.diff > 90 and x.diff <= 120]),
            d_gt_120=len([1 for x in g if x.diff > 120]),
        )
        for g in _items1
    ]


result = _q0()
print(json.dumps(result, separators=(",", ":"), default=lambda o: vars(o)))
test_TPCDS_Q50_simplified()
