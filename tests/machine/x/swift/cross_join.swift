struct CustomerIdId: Equatable {
    var customerId: Int
    var id: Int
    var total: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

var customers = [IdName(id: 1, name: "Alice"), IdName(id: 2, name: "Bob"), IdName(id: 3, name: "Charlie")]
var orders = [CustomerIdId(customerId: 1, id: 100, total: 250), CustomerIdId(customerId: 2, id: 101, total: 125), CustomerIdId(customerId: 1, id: 102, total: 300)]
var result = orders.flatMap { o in customers.map { c in ["orderId": o.id, "orderCustomerId": o.customerId, "pairedCustomerName": c.name, "orderTotal": o.total] } }
print("--- Cross Join: All order-customer pairs ---")
for entry in result as! [[String:Any]] {
    print("Order", entry["orderId"]!, "(customerId:", entry["orderCustomerId"]!, ", total: $", entry["orderTotal"]!, ") paired with", entry["pairedCustomerName"]!)
}
