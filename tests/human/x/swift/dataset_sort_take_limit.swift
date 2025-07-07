struct Product { let name: String; let price: Int }

let products = [
    Product(name: "Laptop", price: 1500),
    Product(name: "Smartphone", price: 900),
    Product(name: "Tablet", price: 600),
    Product(name: "Monitor", price: 300),
    Product(name: "Keyboard", price: 100),
    Product(name: "Mouse", price: 50),
    Product(name: "Headphones", price: 200)
]

let expensive = products.sorted { $0.price > $1.price }
                      .dropFirst(1)
                      .prefix(3)

print("--- Top products (excluding most expensive) ---")
for item in expensive {
    print("\(item.name) costs $\(item.price)")
}
