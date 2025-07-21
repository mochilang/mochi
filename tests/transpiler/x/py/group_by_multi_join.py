from dataclasses import dataclass
from collections import defaultdict

@dataclass
class Nation:
    id: int
    name: str

@dataclass
class Supplier:
    id: int
    nation: int

@dataclass
class PartSupp:
    part: int
    supplier: int
    cost: float
    qty: int

@dataclass
class Filtered:
    part: int
    value: float

@dataclass
class Grouped:
    part: int
    total: float

nations = [
    Nation(1, "A"),
    Nation(2, "B")
]

suppliers = [
    Supplier(1, 1),
    Supplier(2, 2)
]

partsupp = [
    PartSupp(100, 1, 10.0, 2),
    PartSupp(100, 2, 20.0, 1),
    PartSupp(200, 1, 5.0, 3)
]

# Filtered join
filtered = [
    Filtered(ps.part, ps.cost * ps.qty)
    for ps in partsupp
    for s in suppliers
    if s.id == ps.supplier
    for n in nations
    if n.id == s.nation and n.name == "A"
]

# Group by part
grouped_dict = defaultdict(float)
for x in filtered:
    grouped_dict[x.part] += x.value

# Result
grouped = [Grouped(part=k, total=v) for k, v in grouped_dict.items()]

# Output
for g in grouped:
    print(g)
