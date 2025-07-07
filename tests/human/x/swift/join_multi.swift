struct Customer { let id: Int; let name: String }
struct Order { let id: Int; let customerId: Int }
struct Item { let orderId: Int; let sku: String }

let customers = [
    Customer(id: 1, name: "Alice"),
    Customer(id: 2, name: "Bob")
]

let orders = [
    Order(id: 100, customerId: 1),
    Order(id: 101, customerId: 2)
]

let items = [
    Item(orderId: 100, sku: "a"),
    Item(orderId: 101, sku: "b")
]

var result: [(name: String, sku: String)] = []
for o in orders {
    if let c = customers.first(where: { $0.id == o.customerId }),
       let i = items.first(where: { $0.orderId == o.id }) {
        result.append((name: c.name, sku: i.sku))
    }
}

print("--- Multi Join ---")
for r in result {
    print("\(r.name) bought item \(r.sku)")
}
