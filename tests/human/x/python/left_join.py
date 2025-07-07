customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
]
orders = [
    {"id": 100, "customerId": 1, "total": 250},
    {"id": 101, "customerId": 3, "total": 80},
]

result = []
for o in orders:
    c = next((c for c in customers if c["id"] == o["customerId"]), None)
    result.append({"orderId": o["id"], "customer": c, "total": o["total"]})

print("--- Left Join ---")
for entry in result:
    print("Order", entry["orderId"], "customer", entry["customer"], "total", entry["total"])
