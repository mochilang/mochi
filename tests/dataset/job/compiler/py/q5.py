from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    typical_european_movie: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto2:
    ct_id: int
    kind: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto3:
    it_id: int
    info: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto4:
    t_id: int
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
    info: str
    info_type_id: int

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


def test_Q5_finds_the_lexicographically_first_qualifying_title():
    assert result == [Auto1(typical_european_movie="A Film")]


company_type = [
    Auto2(ct_id=1, kind="production companies"),
    Auto2(ct_id=2, kind="other"),
]
info_type = [Auto3(it_id=10, info="languages")]
title = [
    Auto4(t_id=100, title="B Movie", production_year=2010),
    Auto4(t_id=200, title="A Film", production_year=2012),
    Auto4(t_id=300, title="Old Movie", production_year=2000),
]
movie_companies = [
    Auto5(movie_id=100, company_type_id=1, note="ACME (France) (theatrical)"),
    Auto5(movie_id=200, company_type_id=1, note="ACME (France) (theatrical)"),
    Auto5(movie_id=300, company_type_id=1, note="ACME (France) (theatrical)"),
]
movie_info = [
    Auto6(movie_id=100, info="German", info_type_id=10),
    Auto6(movie_id=200, info="Swedish", info_type_id=10),
    Auto6(movie_id=300, info="German", info_type_id=10),
]
candidate_titles = _query(
    company_type,
    [
        {
            "items": movie_companies,
            "on": lambda ct, mc: mc["company_type_id"] == ct["ct_id"],
        },
        {
            "items": movie_info,
            "on": lambda ct, mc, mi: mi["movie_id"] == mc["movie_id"],
        },
        {
            "items": info_type,
            "on": lambda ct, mc, mi, it: it["it_id"] == mi["info_type_id"],
        },
        {"items": title, "on": lambda ct, mc, mi, it, t: t["t_id"] == mc["movie_id"]},
    ],
    {
        "select": lambda ct, mc, mi, it, t: t["title"],
        "where": lambda ct, mc, mi, it, t: (
            (
                (ct["kind"] == "production companies" and "(theatrical)" in mc["note"])
                and "(France)" in mc["note"]
            )
            and t["production_year"] > 2005
        )
        and mi["info"]
        in [
            "Sweden",
            "Norway",
            "Germany",
            "Denmark",
            "Swedish",
            "Denish",
            "Norwegian",
            "German",
        ],
    },
)
result = [Auto1(typical_european_movie=_min(candidate_titles))]
print(json.dumps(result, default=lambda o: vars(o)))
test_Q5_finds_the_lexicographically_first_qualifying_title()