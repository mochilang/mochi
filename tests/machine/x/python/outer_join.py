customers = [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"},
    {"id": 3, "name": "Charlie"},
    {"id": 4, "name": "Diana"},  # Has no order
]
orders = [
    {"id": 100, "customerId": 1, "total": 250},
    {"id": 101, "customerId": 2, "total": 125},
    {"id": 102, "customerId": 1, "total": 300},
    {"id": 103, "customerId": 5, "total": 80},  # Unknown customer
]

result = []
for o in orders:
    c = next((cu for cu in customers if cu["id"] == o["customerId"]), None)
    result.append({"order": o, "customer": c})
for c in customers:
    if not any(o["customerId"] == c["id"] for o in orders):
        result.append({"order": None, "customer": c})

print("--- Outer Join using syntax ---")
for row in result:
    if row["order"]:
        if row["customer"]:
            print("Order", row["order"]["id"], "by", row["customer"]["name"], "- $", row["order"]["total"])
        else:
            print("Order", row["order"]["id"], "by", "Unknown", "- $", row["order"]["total"])
    elif row["customer"]:
        print("Customer", row["customer"]["name"], "has no orders")
