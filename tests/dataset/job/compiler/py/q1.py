from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    production_note: str
    movie_title: str
    movie_year: int

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto2:
    id: int
    kind: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto3:
    id: int
    info: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto4:
    id: int
    title: str
    production_year: int

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto5:
    movie_id: int
    company_type_id: int
    note: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto6:
    movie_id: int
    info_type_id: int

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto7:
    note: object
    title: object
    year: object

    def __getitem__(self, key):
        return getattr(self, key)


from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")


def _min(v):
    if hasattr(v, "Items"):
        v = v.Items
    if not isinstance(v, list):
        raise Exception("min() expects list or group")
    vals = [it for it in v if it is not None]
    if not vals:
        return 0
    return min(vals)


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


def test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production():
    assert result == Auto1(
        production_note="ACME (co-production)",
        movie_title="Good Movie",
        movie_year=1995,
    )


company_type = [
    Auto2(id=1, kind="production companies"),
    Auto2(id=2, kind="distributors"),
]
info_type = [Auto3(id=10, info="top 250 rank"), Auto3(id=20, info="bottom 10 rank")]
title = [
    Auto4(id=100, title="Good Movie", production_year=1995),
    Auto4(id=200, title="Bad Movie", production_year=2000),
]
movie_companies = [
    Auto5(movie_id=100, company_type_id=1, note="ACME (co-production)"),
    Auto5(
        movie_id=200, company_type_id=1, note="MGM (as Metro-Goldwyn-Mayer Pictures)"
    ),
]
movie_info_idx = [
    Auto6(movie_id=100, info_type_id=10),
    Auto6(movie_id=200, info_type_id=20),
]
filtered = _query(
    company_type,
    [
        {
            "items": movie_companies,
            "on": lambda ct, mc: ct["id"] == mc["company_type_id"],
        },
        {"items": title, "on": lambda ct, mc, t: t["id"] == mc["movie_id"]},
        {
            "items": movie_info_idx,
            "on": lambda ct, mc, t, mi: mi["movie_id"] == t["id"],
        },
        {
            "items": info_type,
            "on": lambda ct, mc, t, mi, it: it["id"] == mi["info_type_id"],
        },
    ],
    {
        "select": lambda ct, mc, t, mi, it: Auto7(
            note=mc["note"], title=t["title"], year=t["production_year"]
        ),
        "where": lambda ct, mc, t, mi, it: (
            (ct["kind"] == "production companies" and it["info"] == "top 250 rank")
            and (not "(as Metro-Goldwyn-Mayer Pictures)" in mc["note"])
        )
        and ("(co-production)" in mc["note"] or "(presents)" in mc["note"]),
    },
)
result = Auto1(
    production_note=_min([r.note for r in filtered]),
    movie_title=_min([r.title for r in filtered]),
    movie_year=_min([r.year for r in filtered]),
)
print(json.dumps([result], default=lambda o: vars(o)))
test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production()