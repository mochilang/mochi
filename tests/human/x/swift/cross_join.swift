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
    Order(id: 102, customerId: 1, total: 300)
]

var result: [(orderId: Int, orderCustomerId: Int, pairedCustomerName: String, orderTotal: Int)] = []
for o in orders {
    for c in customers {
        result.append((o.id, o.customerId, c.name, o.total))
    }
}

print("--- Cross Join: All order-customer pairs ---")
for entry in result {
    print("Order \(entry.orderId) (customerId: \(entry.orderCustomerId), total: $\(entry.orderTotal)) paired with \(entry.pairedCustomerName)")
}
