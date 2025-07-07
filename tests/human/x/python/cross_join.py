customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
    {"id": 3, "name": "Charlie"},
]
orders = [
    {"id": 100, "customerId": 1, "total": 250},
    {"id": 101, "customerId": 2, "total": 125},
    {"id": 102, "customerId": 1, "total": 300},
]
result = []
for o in orders:
    for c in customers:
        result.append({
            "orderId": o["id"],
            "orderCustomerId": o["customerId"],
            "pairedCustomerName": c["name"],
            "orderTotal": o["total"],
        })

print("--- Cross Join: All order-customer pairs ---")
for entry in result:
    print(
        "Order",
        entry["orderId"],
        "(customerId:",
        entry["orderCustomerId"],
        ", total: $",
        entry["orderTotal"],
        ") paired with",
        entry["pairedCustomerName"],
    )
