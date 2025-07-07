struct Customer { let id: Int; let name: String }
struct Order { let id: Int; let customerId: Int }

let customers = [
    Customer(id: 1, name: "Alice"),
    Customer(id: 2, name: "Bob")
]
let orders = [
    Order(id: 100, customerId: 1),
    Order(id: 101, customerId: 1),
    Order(id: 102, customerId: 2)
]

var counts: [String: Int] = [:]
for o in orders {
    if let c = customers.first(where: { $0.id == o.customerId }) {
        counts[c.name, default: 0] += 1
    }
}

print("--- Orders per customer ---")
for (name, cnt) in counts {
    print("\(name) orders: \(cnt)")
}
