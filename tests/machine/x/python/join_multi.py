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
    {"orderId": 101, "sku": "b"},
]

result = [
    {"name": c["name"], "sku": i["sku"]}
    for o in orders
    for c in customers
    if c["id"] == o["customerId"]
    for i in items
    if i["orderId"] == o["id"]
]
print("--- Multi Join ---")
for r in result:
    print(r["name"], "bought item", r["sku"])
