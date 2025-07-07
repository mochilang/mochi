struct Customer { let id: Int; let name: String }
struct Order { let id: Int; let customerId: Int; let total: Int }

let customers = [
    Customer(id: 1, name: "Alice"),
    Customer(id: 2, name: "Bob"),
    Customer(id: 3, name: "Charlie"),
    Customer(id: 4, name: "Diana")
]

let orders = [
    Order(id: 100, customerId: 1, total: 250),
    Order(id: 101, customerId: 2, total: 125),
    Order(id: 102, customerId: 1, total: 300)
]

print("--- Right Join using syntax ---")
for c in customers {
    if let o = orders.first(where: { $0.customerId == c.id }) {
        print("Customer", c.name, "has order", o.id, "- $", o.total)
    } else {
        print("Customer", c.name, "has no orders")
    }
}
