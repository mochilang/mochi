let customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"], ["id": 3, "name": "Charlie"]]
let orders = [["id": 100, "customerId": 1, "total": 250], ["id": 101, "customerId": 2, "total": 125], ["id": 102, "customerId": 1, "total": 300], ["id": 103, "customerId": 4, "total": 80]]
let result = ({
	var _res: [Any] = []
	for o in orders {
		for c in customers {
			if !(o["customerId"] as! Int == c["id"] as! Int) { continue }
			_res.append((orderId: o["id"] as! Int, customerName: c["name"] as! String, total: o["total"] as! Int))
		}
	}
	return _res
}())
print("--- Orders with customer info ---")
for entry in result {
    print("Order", entry["orderId"], "by", entry["customerName"], "- $", entry["total"])
}
