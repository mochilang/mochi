from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    alternative_name: str
    character_name: str
    movie: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto10:
    alt: object
    character: object
    movie: object

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
    id: int
    name: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto4:
    person_id: int
    person_role_id: int
    movie_id: int
    role_id: int
    note: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto5:
    id: int
    country_code: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto6:
    movie_id: int
    company_id: int
    note: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto7:
    id: int
    name: str
    gender: str

    def __getitem__(self, key):
        return getattr(self, key)


@dataclasses.dataclass
class Auto8:
    id: int
    role: str

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


def test_Q9_selects_minimal_alternative_name__character_and_movie():
    assert result == [
        Auto1(alternative_name="A. N. G.", character_name="Angel", movie="Famous Film")
    ]


aka_name = [Auto2(person_id=1, name="A. N. G."), Auto2(person_id=2, name="J. D.")]
char_name = [Auto3(id=10, name="Angel"), Auto3(id=20, name="Devil")]
cast_info = [
    Auto4(person_id=1, person_role_id=10, movie_id=100, role_id=1000, note="(voice)"),
    Auto4(person_id=2, person_role_id=20, movie_id=200, role_id=1000, note="(voice)"),
]
company_name = [Auto5(id=100, country_code="[us]"), Auto5(id=200, country_code="[gb]")]
movie_companies = [
    Auto6(movie_id=100, company_id=100, note="ACME Studios (USA)"),
    Auto6(movie_id=200, company_id=200, note="Maple Films"),
]
name = [
    Auto7(id=1, name="Angela Smith", gender="f"),
    Auto7(id=2, name="John Doe", gender="m"),
]
role_type = [Auto8(id=1000, role="actress"), Auto8(id=2000, role="actor")]
title = [
    Auto9(id=100, title="Famous Film", production_year=2010),
    Auto9(id=200, title="Old Movie", production_year=1999),
]
matches = _query(
    aka_name,
    [
        {"items": name, "on": lambda an, n: an["person_id"] == n["id"]},
        {"items": cast_info, "on": lambda an, n, ci: ci["person_id"] == n["id"]},
        {
            "items": char_name,
            "on": lambda an, n, ci, chn: chn["id"] == ci["person_role_id"],
        },
        {"items": title, "on": lambda an, n, ci, chn, t: t["id"] == ci["movie_id"]},
        {
            "items": movie_companies,
            "on": lambda an, n, ci, chn, t, mc: mc["movie_id"] == t["id"],
        },
        {
            "items": company_name,
            "on": lambda an, n, ci, chn, t, mc, cn: cn["id"] == mc["company_id"],
        },
        {
            "items": role_type,
            "on": lambda an, n, ci, chn, t, mc, cn, rt: rt["id"] == ci["role_id"],
        },
    ],
    {
        "select": lambda an, n, ci, chn, t, mc, cn, rt: Auto10(
            alt=an["name"], character=chn["name"], movie=t["title"]
        ),
        "where": lambda an, n, ci, chn, t, mc, cn, rt: (
            (
                (
                    (
                        (
                            (
                                ci["note"]
                                in [
                                    "(voice)",
                                    "(voice: Japanese version)",
                                    "(voice) (uncredited)",
                                    "(voice: English version)",
                                ]
                                and cn["country_code"] == "[us]"
                            )
                            and ("(USA)" in mc["note"] or "(worldwide)" in mc["note"])
                        )
                        and n["gender"] == "f"
                    )
                    and "Ang" in n["name"]
                )
                and rt["role"] == "actress"
            )
            and t["production_year"] >= 2005
        )
        and t["production_year"] <= 2015,
    },
)
result = [
    Auto1(
        alternative_name=_min([x.alt for x in matches]),
        character_name=_min([x.character for x in matches]),
        movie=_min([x.movie for x in matches]),
    )
]
print(json.dumps(result, default=lambda o: vars(o)))
test_Q9_selects_minimal_alternative_name__character_and_movie()