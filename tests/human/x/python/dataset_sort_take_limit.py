products = [
    {"name": "Laptop", "price": 1500},
    {"name": "Smartphone", "price": 900},
    {"name": "Tablet", "price": 600},
    {"name": "Monitor", "price": 300},
    {"name": "Keyboard", "price": 100},
    {"name": "Mouse", "price": 50},
    {"name": "Headphones", "price": 200},
]
expensive = sorted(products, key=lambda p: -p["price"])[1:4]
print("--- Top products (excluding most expensive) ---")
for item in expensive:
    print(item["name"], "costs $", item["price"])
