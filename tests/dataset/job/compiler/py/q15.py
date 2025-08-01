# Generated by Mochi compiler v0.10.27 on 2025-07-17T17:45:22Z
from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    release_date: str
    internet_movie: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto2:
    movie_id: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto3:
    id: int
    country_code: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto4:
    id: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto5:
    id: int
    info: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto6:
    movie_id: int
    company_id: int
    company_type_id: int
    note: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto7:
    movie_id: int
    info_type_id: int
    note: str
    info: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto8:
    movie_id: int
    keyword_id: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto9:
    id: int
    title: str
    production_year: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


from typing import Any, TypeVar, Generic, Callable

T = TypeVar("T")
K = TypeVar("K")
UNDEFINED = object()


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


def test_Q15_finds_the_earliest_US_internet_movie_release_after_2000():
    assert result == [
        Auto1(release_date="USA: March 2005", internet_movie="Example Movie")
    ]


aka_title = [Auto2(movie_id=1), Auto2(movie_id=2)]
company_name = [Auto3(id=1, country_code="[us]"), Auto3(id=2, country_code="[gb]")]
company_type = [Auto4(id=10), Auto4(id=20)]
info_type = [Auto5(id=5, info="release dates"), Auto5(id=6, info="other")]
keyword = [Auto4(id=100), Auto4(id=200)]
movie_companies = [
    Auto6(
        movie_id=1, company_id=1, company_type_id=10, note="release (2005) (worldwide)"
    ),
    Auto6(
        movie_id=2, company_id=2, company_type_id=20, note="release (1999) (worldwide)"
    ),
]
movie_info = [
    Auto7(movie_id=1, info_type_id=5, note="internet", info="USA: March 2005"),
    Auto7(movie_id=2, info_type_id=5, note="theater", info="USA: May 1999"),
]
movie_keyword = [Auto8(movie_id=1, keyword_id=100), Auto8(movie_id=2, keyword_id=200)]
title = [
    Auto9(id=1, title="Example Movie", production_year=2005),
    Auto9(id=2, title="Old Movie", production_year=1999),
]
rows = _query(
    title,
    [
        {"items": aka_title, "on": lambda t, at: at["movie_id"] == t["id"]},
        {"items": movie_info, "on": lambda t, at, mi: mi["movie_id"] == t["id"]},
        {"items": movie_keyword, "on": lambda t, at, mi, mk: mk["movie_id"] == t["id"]},
        {
            "items": movie_companies,
            "on": lambda t, at, mi, mk, mc: mc["movie_id"] == t["id"],
        },
        {
            "items": keyword,
            "on": lambda t, at, mi, mk, mc, k: k["id"] == mk["keyword_id"],
        },
        {
            "items": info_type,
            "on": lambda t, at, mi, mk, mc, k, it1: it1["id"] == mi["info_type_id"],
        },
        {
            "items": company_name,
            "on": lambda t, at, mi, mk, mc, k, it1, cn: cn["id"] == mc["company_id"],
        },
        {
            "items": company_type,
            "on": lambda t, at, mi, mk, mc, k, it1, cn, ct: ct["id"]
            == mc["company_type_id"],
        },
    ],
    {
        "select": lambda t, at, mi, mk, mc, k, it1, cn, ct: Auto1(
            release_date=mi["info"], internet_movie=t["title"]
        ),
        "where": lambda t, at, mi, mk, mc, k, it1, cn, ct: (
            (
                (
                    (
                        (
                            (
                                cn["country_code"] == "[us]"
                                and it1["info"] == "release dates"
                            )
                            and "200" in mc["note"]
                        )
                        and "worldwide" in mc["note"]
                    )
                    and "internet" in mi["note"]
                )
                and "USA:" in mi["info"]
            )
            and "200" in mi["info"]
        )
        and t["production_year"] > 2000,
    },
)
result = [
    Auto1(
        release_date=_min([r.release_date for r in rows]),
        internet_movie=_min([r.internet_movie for r in rows]),
    )
]
print(json.dumps(result, separators=(",", ":"), default=lambda o: vars(o)))
test_Q15_finds_the_earliest_US_internet_movie_release_after_2000()
