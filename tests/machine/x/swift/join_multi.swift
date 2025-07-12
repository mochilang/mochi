struct CustomerIdId: Equatable {
    var customerId: Int
    var id: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

struct OrderIdSku: Equatable {
    var orderId: Int
    var sku: String
}

var customers = [IdName(id: 1, name: "Alice"), IdName(id: 2, name: "Bob")]
var orders = [CustomerIdId(customerId: 1, id: 100), CustomerIdId(customerId: 2, id: 101)]
var items = [OrderIdSku(orderId: 100, sku: "a"), OrderIdSku(orderId: 101, sku: "b")]
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
