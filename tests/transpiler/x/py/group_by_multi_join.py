from collections import defaultdict
from dataclasses import dataclass

@dataclass
class Nation:
    id: int
    name: str

nations = [Nation(1, "A"), Nation(2, "B")]
@dataclass
class Supplier:
    id: int
    nation: int

suppliers = [Supplier(1, 1), Supplier(2, 2)]
@dataclass
class PartSupp:
    part: int
    supplier: int
    cost: float
    qty: int

partsupp = [PartSupp(100, 1, 10.0, 2), PartSupp(100, 2, 20.0, 1), PartSupp(200, 1, 5.0, 3)]
@dataclass
class Filtered:
    part: int
    value: float

filtered = []
for ps in partsupp:
    for s in suppliers:
        if s.id == ps.supplier:
            for n in nations:
                if n.id == s.nation and n.name == "A":
                    filtered.append(Filtered(ps.part, ps.cost * ps.qty))
@dataclass
class GenType1:
    part: int
    total: float

grouped_dict = defaultdict(float)
for x in filtered:
    grouped_dict[x.part] += x.value
grouped = [GenType1(part=k, total=v) for k, v in grouped_dict.items()]
for g in grouped:
    if g.total.is_integer():
        g.total = int(g.total)
print("[" + ", ".join(f"GenType1 {{part = {g.part}, total = {g.total}}}" for g in grouped) + "]")
