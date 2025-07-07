struct Customer { let id: Int; let name: String }
struct Order { let id: Int; let customerId: Int }

let customers = [
    Customer(id: 1, name: "Alice"),
    Customer(id: 2, name: "Bob"),
    Customer(id: 3, name: "Charlie") // No orders
]
let orders = [
    Order(id: 100, customerId: 1),
    Order(id: 101, customerId: 1),
    Order(id: 102, customerId: 2)
]

var stats: [(name: String, count: Int)] = []
for c in customers {
    let cnt = orders.filter { $0.customerId == c.id }.count
    stats.append((name: c.name, count: cnt))
}

print("--- Group Left Join ---")
for s in stats {
    print("\(s.name) orders: \(s.count)")
}
