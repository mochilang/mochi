from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    of_person: str
    biography_movie: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto10:
    person_name: object
    movie_title: object

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto2:
    person_id: int
    name: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto3:
    person_id: int
    movie_id: int

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto4:
    id: int
    info: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto5:
    id: int
    link: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto6:
    linked_movie_id: int
    link_type_id: int

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto7:
    id: int
    name: str
    name_pcode_cf: str
    gender: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto8:
    person_id: int
    info_type_id: int
    note: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto9:
    id: int
    title: str
    production_year: int

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


def test_Q7_finds_movie_features_biography_for_person():
    assert result == [Auto1(of_person="Alan Brown", biography_movie="Feature Film")]


aka_name = [Auto2(person_id=1, name="Anna Mae"), Auto2(person_id=2, name="Chris")]
cast_info = [Auto3(person_id=1, movie_id=10), Auto3(person_id=2, movie_id=20)]
info_type = [Auto4(id=1, info="mini biography"), Auto4(id=2, info="trivia")]
link_type = [Auto5(id=1, link="features"), Auto5(id=2, link="references")]
movie_link = [
    Auto6(linked_movie_id=10, link_type_id=1),
    Auto6(linked_movie_id=20, link_type_id=2),
]
name = [
    Auto7(id=1, name="Alan Brown", name_pcode_cf="B", gender="m"),
    Auto7(id=2, name="Zoe", name_pcode_cf="Z", gender="f"),
]
person_info = [
    Auto8(person_id=1, info_type_id=1, note="Volker Boehm"),
    Auto8(person_id=2, info_type_id=1, note="Other"),
]
title = [
    Auto9(id=10, title="Feature Film", production_year=1990),
    Auto9(id=20, title="Late Film", production_year=2000),
]
rows = _query(
    aka_name,
    [
        {"items": name, "on": lambda an, n: n["id"] == an["person_id"]},
        {
            "items": person_info,
            "on": lambda an, n, pi: pi["person_id"] == an["person_id"],
        },
        {
            "items": info_type,
            "on": lambda an, n, pi, it: it["id"] == pi["info_type_id"],
        },
        {
            "items": cast_info,
            "on": lambda an, n, pi, it, ci: ci["person_id"] == n["id"],
        },
        {"items": title, "on": lambda an, n, pi, it, ci, t: t["id"] == ci["movie_id"]},
        {
            "items": movie_link,
            "on": lambda an, n, pi, it, ci, t, ml: ml["linked_movie_id"] == t["id"],
        },
        {
            "items": link_type,
            "on": lambda an, n, pi, it, ci, t, ml, lt: lt["id"] == ml["link_type_id"],
        },
    ],
    {
        "select": lambda an, n, pi, it, ci, t, ml, lt: Auto10(
            person_name=n["name"], movie_title=t["title"]
        ),
        "where": lambda an, n, pi, it, ci, t, ml, lt: (
            (
                (
                    (
                        (
                            (
                                (
                                    (
                                        (
                                            (
                                                (
                                                    "a" in an["name"]
                                                    and it["info"] == "mini biography"
                                                )
                                                and lt["link"] == "features"
                                            )
                                            and n["name_pcode_cf"] >= "A"
                                        )
                                        and n["name_pcode_cf"] <= "F"
                                    )
                                    and (
                                        n["gender"] == "m"
                                        or (
                                            n["gender"] == "f"
                                            and str(n["name"]).startswith("B")
                                        )
                                    )
                                )
                                and pi["note"] == "Volker Boehm"
                            )
                            and t["production_year"] >= 1980
                        )
                        and t["production_year"] <= 1995
                    )
                    and pi["person_id"] == an["person_id"]
                )
                and pi["person_id"] == ci["person_id"]
            )
            and an["person_id"] == ci["person_id"]
        )
        and ci["movie_id"] == ml["linked_movie_id"],
    },
)
result = [
    Auto1(
        of_person=_min([r.person_name for r in rows]),
        biography_movie=_min([r.movie_title for r in rows]),
    )
]
print(json.dumps(result, default=lambda o: vars(o)))
test_Q7_finds_movie_features_biography_for_person()