customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
    {"id": 3, "name": "Charlie"},
    {"id": 4, "name": "Diana"},
]
orders = [
    {"id": 100, "customerId": 1, "total": 250},
    {"id": 101, "customerId": 2, "total": 125},
    {"id": 102, "customerId": 1, "total": 300},
]

result = []
for c in customers:
    ords = [o for o in orders if o["customerId"] == c["id"]]
    if ords:
        for o in ords:
            result.append({"customerName": c["name"], "order": o})
    else:
        result.append({"customerName": c["name"], "order": None})

print("--- Right Join using syntax ---")
for entry in result:
    if entry["order"]:
        print(
            "Customer",
            entry["customerName"],
            "has order",
            entry["order"]["id"],
            "- $",
            entry["order"]["total"],
        )
    else:
        print("Customer", entry["customerName"], "has no orders")
