customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
    {"id": 3, "name": "Charlie"},
]
orders = [
    {"id": 100, "customerId": 1},
    {"id": 101, "customerId": 1},
    {"id": 102, "customerId": 2},
]

result = []
for c in customers:
    count = sum(1 for o in orders if o["customerId"] == c["id"])
    result.append({"name": c["name"], "count": count})

print("--- Group Left Join ---")
for s in result:
    print(s["name"], "orders:", s["count"])
