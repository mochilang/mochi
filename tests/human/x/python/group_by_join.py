customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
]
orders = [
    {"id": 100, "customerId": 1},
    {"id": 101, "customerId": 1},
    {"id": 102, "customerId": 2},
]

# Join orders with customers and count per customer
stats = {}
for o in orders:
    c = next(c for c in customers if c["id"] == o["customerId"])
    stats[c["name"]] = stats.get(c["name"], 0) + 1

result = [{"name": name, "count": cnt} for name, cnt in stats.items()]
print("--- Orders per customer ---")
for s in result:
    print(s["name"], "orders:", s["count"])
