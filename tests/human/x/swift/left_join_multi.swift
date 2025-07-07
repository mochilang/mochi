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
    Item(orderId: 100, sku: "a")
]

var result: [(orderId: Int, name: String, item: Item?)] = []
for o in orders {
    if let c = customers.first(where: { $0.id == o.customerId }) {
        let i = items.first(where: { $0.orderId == o.id })
        result.append((orderId: o.id, name: c.name, item: i))
    }
}

print("--- Left Join Multi ---")
for r in result {
    print(r.orderId, r.name, r.item as Any)
}
