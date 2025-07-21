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

@dataclass
class StatsGroup:
    key: str
    items: list

_stats_groups = {}
for person in people:
    g = _stats_groups.setdefault(person.city, StatsGroup(person.city, []))
    g.items.append(person)
stats = []
for g in _stats_groups.values():
    stats.append(Stat(city=g.key, count=len(g.items), avg_age=sum(p.age for p in g.items) / len(g.items)))
print("--- People grouped by city ---")
for s in stats:
    print(f"{s.city}: count = {s.count}, avg_age = {s.avg_age}")
