var products = [["name": "Laptop", "price": 1500], ["name": "Smartphone", "price": 900], ["name": "Tablet", "price": 600], ["name": "Monitor", "price": 300], ["name": "Keyboard", "price": 100], ["name": "Mouse", "price": 50], ["name": "Headphones", "price": 200]]
var expensive = products.map { p in (value: p, key: p["price"] as! Int) }.sorted { $0.key > $1.key }.dropFirst(1).prefix(3).map { $0.value }
print("--- Top products (excluding most expensive) ---")
for item in expensive as! [[String:Any]] {
    print(item["name"] as! String, "costs $", item["price"] as! Int)
}
