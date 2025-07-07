struct Customer { let id: Int; let name: String }
struct Order { let id: Int; let customerId: Int; let total: Int }

let customers = [
    Customer(id: 1, name: "Alice"),
    Customer(id: 2, name: "Bob"),
    Customer(id: 3, name: "Charlie")
]
let orders = [
    Order(id: 100, customerId: 1, total: 250),
    Order(id: 101, customerId: 2, total: 125),
    Order(id: 102, customerId: 1, total: 300),
    Order(id: 103, customerId: 4, total: 80)
]

var result: [(orderId: Int, customerName: String, total: Int)] = []
for o in orders {
    if let c = customers.first(where: { $0.id == o.customerId }) {
        result.append((orderId: o.id, customerName: c.name, total: o.total))
    }
}

print("--- Orders with customer info ---")
for entry in result {
    print("Order \(entry.orderId) by \(entry.customerName) - $\(entry.total)")
}
