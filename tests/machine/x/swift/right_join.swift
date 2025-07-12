struct Auto1: Equatable {
    var id: Int
    var name: String
}

struct Auto2: Equatable {
    var customerId: Int
    var id: Int
    var total: Int
}

var customers = [Auto1(id: 1, name: "Alice"), Auto1(id: 2, name: "Bob"), Auto1(id: 3, name: "Charlie"), Auto1(id: 4, name: "Diana")]
var orders = [Auto2(customerId: 1, id: 100, total: 250), Auto2(customerId: 2, id: 101, total: 125), Auto2(customerId: 1, id: 102, total: 300)]
var result = ({
	var _res: [[String:Any]] = []
	let _src = customers
	let _join = orders
	for o in _join {
		var _m = false
		for c in _src {
			if !(o.customerId == c.id) { continue }
			_m = true
			_res.append(["customerName": c.name, "order": o])
		}
		if !_m {
			let c: Any? = nil
			_res.append(["customerName": nil, "order": o])
		}
	}
	return _res
}())
print("--- Right Join using syntax ---")
for entry in result as! [[String:Any]] {
    if entry["order"]! != nil {
        print("Customer", entry["customerName"]!, "has order", entry["order"]!["id"]!, "- $", entry["order"]!["total"]!)
    }
    else {
        print("Customer", entry["customerName"]!, "has no orders")
    }
}
