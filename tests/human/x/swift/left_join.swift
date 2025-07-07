struct Customer { let id: Int; let name: String }
struct Order { let id: Int; let customerId: Int; let total: Int }

let customers = [
    Customer(id: 1, name: "Alice"),
    Customer(id: 2, name: "Bob")
]
let orders = [
    Order(id: 100, customerId: 1, total: 250),
    Order(id: 101, customerId: 3, total: 80)
]

var result: [(orderId: Int, customer: Customer?, total: Int)] = []
for o in orders {
    let c = customers.first(where: { $0.id == o.customerId })
    result.append((orderId: o.id, customer: c, total: o.total))
}

print("--- Left Join ---")
for entry in result {
    let name = entry.customer?.name ?? "nil"
    print("Order \(entry.orderId) customer \(name) total \(entry.total)")
}
