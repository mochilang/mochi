struct Auto1: Equatable {
    var id: Int
    var name: String
}

struct Auto2: Equatable {
    var customerId: Int
    var id: Int
}

struct Auto3: Equatable {
    var orderId: Int
    var sku: String
}

var customers = [Auto1(id: 1, name: "Alice"), Auto1(id: 2, name: "Bob")]
var orders = [Auto2(customerId: 1, id: 100), Auto2(customerId: 2, id: 101)]
var items = [Auto3(orderId: 100, sku: "a"), Auto3(orderId: 101, sku: "b")]
var result = ({
	var _res: [[String:Any]] = []
	for o in orders {
		for c in customers {
			if !(o.customerId == c.id) { continue }
			for i in items {
				if !(o.id == i.orderId) { continue }
				_res.append(["name": c.name, "sku": i.sku])
			}
		}
	}
	return _res
}())
print("--- Multi Join ---")
for r in result as! [[String:Any]] {
    print(r["name"]!, "bought item", r["sku"]!)
}
