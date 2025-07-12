struct CustomerIdId: Equatable {
    var customerId: Int
    var id: Int
    var total: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

var customers = [IdName(id: 1, name: "Alice"), IdName(id: 2, name: "Bob"), IdName(id: 3, name: "Charlie"), IdName(id: 4, name: "Diana")]
var orders = [CustomerIdId(customerId: 1, id: 100, total: 250), CustomerIdId(customerId: 2, id: 101, total: 125), CustomerIdId(customerId: 1, id: 102, total: 300)]
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
			_res.append(["customerName": c.name, "order": o])
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
