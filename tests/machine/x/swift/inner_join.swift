struct Auto1: Equatable {
    var id: Int
    var name: String
}

struct Auto2: Equatable {
    var customerId: Int
    var id: Int
    var total: Int
}

var customers = [Auto1(id: 1, name: "Alice"), Auto1(id: 2, name: "Bob"), Auto1(id: 3, name: "Charlie")]
var orders = [Auto2(customerId: 1, id: 100, total: 250), Auto2(customerId: 2, id: 101, total: 125), Auto2(customerId: 1, id: 102, total: 300), Auto2(customerId: 4, id: 103, total: 80)]
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
