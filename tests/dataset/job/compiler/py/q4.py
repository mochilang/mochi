from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    rating: str
    movie_title: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto2:
    id: int
    info: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto3:
    id: int
    keyword: str

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
    keyword_id: int

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto6:
    movie_id: int
    info_type_id: int
    info: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto7:
    rating: object
    title: object

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


def test_Q4_returns_minimum_rating_and_title_for_sequels():
    assert result == [Auto1(rating="6.2", movie_title="Alpha Movie")]


info_type = [Auto2(id=1, info="rating"), Auto2(id=2, info="other")]
keyword = [Auto3(id=1, keyword="great sequel"), Auto3(id=2, keyword="prequel")]
title = [
    Auto4(id=10, title="Alpha Movie", production_year=2006),
    Auto4(id=20, title="Beta Film", production_year=2007),
    Auto4(id=30, title="Old Film", production_year=2004),
]
movie_keyword = [
    Auto5(movie_id=10, keyword_id=1),
    Auto5(movie_id=20, keyword_id=1),
    Auto5(movie_id=30, keyword_id=1),
]
movie_info_idx = [
    Auto6(movie_id=10, info_type_id=1, info="6.2"),
    Auto6(movie_id=20, info_type_id=1, info="7.8"),
    Auto6(movie_id=30, info_type_id=1, info="4.5"),
]
rows = _query(
    info_type,
    [
        {"items": movie_info_idx, "on": lambda it, mi: it["id"] == mi["info_type_id"]},
        {"items": title, "on": lambda it, mi, t: t["id"] == mi["movie_id"]},
        {"items": movie_keyword, "on": lambda it, mi, t, mk: mk["movie_id"] == t["id"]},
        {"items": keyword, "on": lambda it, mi, t, mk, k: k["id"] == mk["keyword_id"]},
    ],
    {
        "select": lambda it, mi, t, mk, k: Auto7(rating=mi["info"], title=t["title"]),
        "where": lambda it, mi, t, mk, k: (
            (
                (it["info"] == "rating" and "sequel" in k["keyword"])
                and mi["info"] > "5.0"
            )
            and t["production_year"] > 2005
        )
        and mk["movie_id"] == mi["movie_id"],
    },
)
result = [
    Auto1(
        rating=_min([r.rating for r in rows]), movie_title=_min([r.title for r in rows])
    )
]
print(json.dumps(result, default=lambda o: vars(o)))
test_Q4_returns_minimum_rating_and_title_for_sequels()