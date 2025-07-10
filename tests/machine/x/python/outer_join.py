from __future__ import annotations
from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")


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


customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
    {"id": 3, "name": "Charlie"},
    {"id": 4, "name": "Diana"},
]
orders = [
    {"id": 100, "customerId": 1, "total": 250},
    {"id": 101, "customerId": 2, "total": 125},
    {"id": 102, "customerId": 1, "total": 300},
    {"id": 103, "customerId": 5, "total": 80},
]
result = _query(
    orders,
    [
        {
            "items": customers,
            "on": lambda o, c: o["customerId"] == c["id"],
            "left": True,
            "right": True,
        }
    ],
    {"select": lambda o, c: {"order": o, "customer": c}},
)
print("--- Outer Join using syntax ---")
for row in result:
    if row["order"]:
        if row["customer"]:
            print(
                "Order",
                row["order"]["id"],
                "by",
                row["customer"]["name"],
                "- $",
                row["order"]["total"],
            )
        else:
            print(
                "Order",
                row["order"]["id"],
                "by",
                "Unknown",
                "- $",
                row["order"]["total"],
            )
    else:
        print("Customer", row["customer"]["name"], "has no orders")
