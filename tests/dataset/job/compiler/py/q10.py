# Generated by Mochi compiler v0.10.27 on 2025-07-17T17:45:18Z
from __future__ import annotations
import dataclasses
import json


@dataclasses.dataclass
class Auto1:
    uncredited_voiced_character: str
    russian_movie: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto2:
    id: int
    name: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto3:
    movie_id: int
    person_role_id: int
    role_id: int
    note: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto4:
    id: int
    country_code: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto5:
    id: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto6:
    movie_id: int
    company_id: int
    company_type_id: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto7:
    id: int
    role: str

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto8:
    id: int
    title: str
    production_year: int

    def __getitem__(self, key):
        return getattr(self, key)

    def __contains__(self, key):
        return hasattr(self, key)


@dataclasses.dataclass
class Auto9:
    character: object
    movie: object

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


def test_Q10_finds_uncredited_voice_actor_in_Russian_movie():
    assert result == [
        Auto1(uncredited_voiced_character="Ivan", russian_movie="Vodka Dreams")
    ]


char_name = [Auto2(id=1, name="Ivan"), Auto2(id=2, name="Alex")]
cast_info = [
    Auto3(
        movie_id=10, person_role_id=1, role_id=1, note="Soldier (voice) (uncredited)"
    ),
    Auto3(movie_id=11, person_role_id=2, role_id=1, note="(voice)"),
]
company_name = [Auto4(id=1, country_code="[ru]"), Auto4(id=2, country_code="[us]")]
company_type = [Auto5(id=1), Auto5(id=2)]
movie_companies = [
    Auto6(movie_id=10, company_id=1, company_type_id=1),
    Auto6(movie_id=11, company_id=2, company_type_id=1),
]
role_type = [Auto7(id=1, role="actor"), Auto7(id=2, role="director")]
title = [
    Auto8(id=10, title="Vodka Dreams", production_year=2006),
    Auto8(id=11, title="Other Film", production_year=2004),
]
matches = _query(
    char_name,
    [
        {"items": cast_info, "on": lambda chn, ci: chn["id"] == ci["person_role_id"]},
        {"items": role_type, "on": lambda chn, ci, rt: rt["id"] == ci["role_id"]},
        {"items": title, "on": lambda chn, ci, rt, t: t["id"] == ci["movie_id"]},
        {
            "items": movie_companies,
            "on": lambda chn, ci, rt, t, mc: mc["movie_id"] == t["id"],
        },
        {
            "items": company_name,
            "on": lambda chn, ci, rt, t, mc, cn: cn["id"] == mc["company_id"],
        },
        {
            "items": company_type,
            "on": lambda chn, ci, rt, t, mc, cn, ct: ct["id"] == mc["company_type_id"],
        },
    ],
    {
        "select": lambda chn, ci, rt, t, mc, cn, ct: Auto9(
            character=chn["name"], movie=t["title"]
        ),
        "where": lambda chn, ci, rt, t, mc, cn, ct: (
            (
                ("(voice)" in ci["note"] and "(uncredited)" in ci["note"])
                and cn["country_code"] == "[ru]"
            )
            and rt["role"] == "actor"
        )
        and t["production_year"] > 2005,
    },
)
result = [
    Auto1(
        uncredited_voiced_character=_min([x.character for x in matches]),
        russian_movie=_min([x.movie for x in matches]),
    )
]
print(json.dumps(result, separators=(",", ":"), default=lambda o: vars(o)))
test_Q10_finds_uncredited_voice_actor_in_Russian_movie()
