struct CustomerIdId: Equatable {
    var customerId: Int
    var id: Int
    var total: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

var customers = [IdName(id: 1, name: "Alice"), IdName(id: 2, name: "Bob")]
var orders = [CustomerIdId(customerId: 1, id: 100, total: 250), CustomerIdId(customerId: 3, id: 101, total: 80)]
var result = ({
	var _res: [[String:Any]] = []
	let _src = orders
	let _join = customers
	for o in _src {
		var _m = false
		for c in _join {
			if !(o.customerId == c.id) { continue }
			_m = true
			_res.append(["orderId": o.id, "customer": c, "total": o.total])
		}
		if !_m {
			let c: Any? = nil
			_res.append(["orderId": o.id, "customer": c, "total": o.total])
		}
	}
	return _res
}())
print("--- Left Join ---")
for entry in result as! [[String:Any]] {
    print("Order", entry["orderId"]!, "customer", entry["customer"]!, "total", entry["total"]!)
}
