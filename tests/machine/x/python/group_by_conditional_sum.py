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


class _Group(Generic[K, T]):

    def __init__(self, key: K):
        self.key = key
        self.Items: list[T] = []
        self.items = self.Items

    def __iter__(self):
        return iter(self.Items)


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


def _sort_key(k):
    if isinstance(k, (list, tuple, dict)):
        return str(k)
    return k


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


items = [
    {"cat": "a", "val": 10, "flag": True},
    {"cat": "a", "val": 5, "flag": False},
    {"cat": "b", "val": 20, "flag": True},
]


def _q0():
    _src = items
    _rows = _query(_src, [], {"select": lambda i: i})
    _groups = _group_by(_rows, lambda i: i["cat"])
    _items1 = _groups
    _items1 = sorted(_items1, key=lambda g: _sort_key(_get(g, "key")))
    return [
        {
            "cat": _get(g, "key"),
            "share": _sum([x["val"] if x["flag"] else 0 for x in g])
            / _sum([x["val"] for x in g]),
        }
        for g in _items1
    ]


result = _q0()
print(result)
