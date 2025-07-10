var customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"], ["id": 3, "name": "Charlie"]]
var orders = [["id": 100, "customerId": 1, "total": 250], ["id": 101, "customerId": 2, "total": 125], ["id": 102, "customerId": 1, "total": 300]]
var result = orders.flatMap { o in customers.map { c in ["orderId": o["id"] as! Int, "orderCustomerId": o["customerId"] as! Int, "pairedCustomerName": c["name"] as! String, "orderTotal": o["total"] as! Int] } }
print("--- Cross Join: All order-customer pairs ---")
for entry in result {
    print("Order", entry["orderId"], "(customerId:", entry["orderCustomerId"], ", total: $", entry["orderTotal"], ") paired with", entry["pairedCustomerName"])
}
