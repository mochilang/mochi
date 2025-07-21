from dataclasses import dataclass
import json

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
    count: int
    total_age: int

_stats_groups = {}
for person in people:
    g = _stats_groups.setdefault(person.city, StatsGroup(person.city, 0, 0))
    g.count += 1
    g.total_age += person.age
stats = []
for g in _stats_groups.values():
    stats.append(Stat(city=g.key, count=g.count, avg_age=g.total_age / g.count))
print("--- People grouped by city ---")
for s in stats:
    print(f"{json.dumps(s.city)} : count = {s.count} , avg_age = {s.avg_age}")
