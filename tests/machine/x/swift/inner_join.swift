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
var orders = [CustomerIdId(customerId: 1, id: 100, total: 250), CustomerIdId(customerId: 2, id: 101, total: 125), CustomerIdId(customerId: 1, id: 102, total: 300), CustomerIdId(customerId: 4, id: 103, total: 80)]
var result = ({
	var _res: [[String:Any]] = []
	for o in orders {
		for c in customers {
			if !(o.customerId == c.id) { continue }
			_res.append(["orderId": o.id, "customerName": c.name, "total": o.total])
		}
	}
	return _res
}())
print("--- Orders with customer info ---")
for entry in result as! [[String:Any]] {
    print("Order", entry["orderId"]!, "by", entry["customerName"]!, "- $", entry["total"]!)
}
