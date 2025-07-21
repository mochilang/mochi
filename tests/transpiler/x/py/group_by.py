from collections import defaultdict
from dataclasses import dataclass

@dataclass
class Person:
    name: str
    age: int
    city: str

people = [Person("Alice", 30, "Paris"), Person("Bob", 15, "Hanoi"), Person("Charlie", 65, "Paris"), Person("Diana", 45, "Hanoi"), Person("Eve", 70, "Paris"), Person("Frank", 22, "Hanoi")]
@dataclass
class Stat:
    city: str
    count: int
    avg_age: float

_stats_groups = defaultdict(list)
for person in people:
    _stats_groups[person.city].append(person)
stats = []
for _p in _stats_groups.items():
    stats.append(Stat(city=_p[0], count=len(_p[1]), avg_age=sum(p.age for p in _p[1]) / len(_p[1])))
print("--- People grouped by city ---")
for s in stats:
    print(f"{s.city}: count = {s.count}, avg_age = {s.avg_age}")
