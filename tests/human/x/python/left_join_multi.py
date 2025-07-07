customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
]
orders = [
    {"id": 100, "customerId": 1},
    {"id": 101, "customerId": 2},
]
items = [
    {"orderId": 100, "sku": "a"},
]

result = []
for o in orders:
    c = next(c for c in customers if c["id"] == o["customerId"])
    item = next((i for i in items if i["orderId"] == o["id"]), None)
    result.append({"orderId": o["id"], "name": c["name"], "item": item})

print("--- Left Join Multi ---")
for r in result:
    print(r["orderId"], r["name"], r["item"])
