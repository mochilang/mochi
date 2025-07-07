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
    Order(id: 102, customerId: 1, total: 300),
    Order(id: 103, customerId: 5, total: 80)
]

var seenCustomers: Set<Int> = []
print("--- Outer Join using syntax ---")
for o in orders {
    if let c = customers.first(where: { $0.id == o.customerId }) {
        print("Order", o.id, "by", c.name, "- $", o.total)
        seenCustomers.insert(c.id)
    } else {
        print("Order", o.id, "by", "Unknown", "- $", o.total)
    }
}
for c in customers {
    if !seenCustomers.contains(c.id) {
        print("Customer", c.name, "has no orders")
    }
}
