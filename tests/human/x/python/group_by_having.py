import json

people = [
    {"name": "Alice", "city": "Paris"},
    {"name": "Bob", "city": "Hanoi"},
    {"name": "Charlie", "city": "Paris"},
    {"name": "Diana", "city": "Hanoi"},
    {"name": "Eve", "city": "Paris"},
    {"name": "Frank", "city": "Hanoi"},
    {"name": "George", "city": "Paris"},
]

stats = {}
for p in people:
    stats.setdefault(p["city"], []).append(p)

big = [
    {"city": city, "num": len(lst)}
    for city, lst in stats.items()
    if len(lst) >= 4
]
print(json.dumps(big))
