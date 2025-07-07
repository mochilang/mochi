customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
    {"id": 3, "name": "Charlie"},
]
orders = [
    {"id": 100, "customerId": 1, "total": 250},
    {"id": 101, "customerId": 2, "total": 125},
    {"id": 102, "customerId": 1, "total": 300},
    {"id": 103, "customerId": 4, "total": 80},
]

result = [
    {
        "orderId": o["id"],
        "customerName": c["name"],
        "total": o["total"],
    }
    for o in orders
    for c in customers
    if c["id"] == o["customerId"]
]
print("--- Orders with customer info ---")
for entry in result:
    print("Order", entry["orderId"], "by", entry["customerName"], "- $", entry["total"])
