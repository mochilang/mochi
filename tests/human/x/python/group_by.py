people = [
    {"name": "Alice", "age": 30, "city": "Paris"},
    {"name": "Bob", "age": 15, "city": "Hanoi"},
    {"name": "Charlie", "age": 65, "city": "Paris"},
    {"name": "Diana", "age": 45, "city": "Hanoi"},
    {"name": "Eve", "age": 70, "city": "Paris"},
    {"name": "Frank", "age": 22, "city": "Hanoi"},
]

# Group people by city and compute count and average age
stats = {}
for p in people:
    g = stats.setdefault(p["city"], {"total": 0, "count": 0})
    g["total"] += p["age"]
    g["count"] += 1

result = [
    {"city": city, "count": g["count"], "avg_age": g["total"] / g["count"]}
    for city, g in stats.items()
]

print("--- People grouped by city ---")
for s in result:
    print(f"{s['city']}: count = {s['count']}, avg_age = {s['avg_age']}")
