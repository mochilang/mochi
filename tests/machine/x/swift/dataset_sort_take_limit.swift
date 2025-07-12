struct NamePrice: Equatable {
    var name: String
    var price: Int
}

var products = [NamePrice(name: "Laptop", price: 1500), NamePrice(name: "Smartphone", price: 900), NamePrice(name: "Tablet", price: 600), NamePrice(name: "Monitor", price: 300), NamePrice(name: "Keyboard", price: 100), NamePrice(name: "Mouse", price: 50), NamePrice(name: "Headphones", price: 200)]
var expensive = products.map { p in (value: p, key: p.price) }.sorted { $0.key > $1.key }.dropFirst(1).prefix(3).map { $0.value }
print("--- Top products (excluding most expensive) ---")
for item in expensive {
    print(item.name, "costs $", item.price)
}
