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
var items = [Auto3(orderId: 100, sku: "a")]
var result = ({
	var _res: [[String:Any]] = []
	for o in orders {
		for c in customers {
			if !(o.customerId == c.id) { continue }
			for i in items {
				if !(o.id == i.orderId) { continue }
				_res.append(["orderId": o.id, "name": c.name, "item": i])
			}
		}
	}
	return _res
}())
print("--- Left Join Multi ---")
for r in result as! [[String:Any]] {
    print(r["orderId"]!, r["name"]!, r["item"]!)
}
