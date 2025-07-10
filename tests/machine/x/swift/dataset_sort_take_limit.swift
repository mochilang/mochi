struct Auto1: Equatable {
    var name: String
    var price: Int
}

var products = [Auto1(name: "Laptop", price: 1500), Auto1(name: "Smartphone", price: 900), Auto1(name: "Tablet", price: 600), Auto1(name: "Monitor", price: 300), Auto1(name: "Keyboard", price: 100), Auto1(name: "Mouse", price: 50), Auto1(name: "Headphones", price: 200)]
var expensive = products.map { p in (value: p, key: p.price) }.sorted { $0.key > $1.key }.dropFirst(1).prefix(3).map { $0.value }
print("--- Top products (excluding most expensive) ---")
for item in expensive {
    print(item.name, "costs $", item.price)
}
