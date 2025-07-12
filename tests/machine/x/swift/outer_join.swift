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
var orders = [CustomerIdId(customerId: 1, id: 100, total: 250), CustomerIdId(customerId: 2, id: 101, total: 125), CustomerIdId(customerId: 1, id: 102, total: 300), CustomerIdId(customerId: 5, id: 103, total: 80)]
var result = ({
	var _res: [[String:Any]] = []
	let _src = orders
	let _join = customers
	var _matched = Array(repeating: false, count: _join.count)
	for o in _src {
		var _m = false
		for (ri, c) in _join.enumerated() {
			if !(o.customerId == c.id) { continue }
			_matched[ri] = true
			_m = true
			_res.append(["order": o, "customer": c])
		}
		if !_m {
			let c: Any? = nil
			_res.append(["order": o, "customer": c])
		}
	}
	for (ri, c) in _join.enumerated() {
		if !_matched[ri] {
			let o: Any? = nil
			let c = c
			_res.append(["order": o, "customer": c])
		}
	}
	return _res
}())
print("--- Outer Join using syntax ---")
for row in result as! [[String:Any]] {
    if row["order"]! != nil {
        if row["customer"]! != nil {
            print("Order", row["order"]!["id"]!, "by", row["customer"]!["name"]!, "- $", row["order"]!["total"]!)
        }
        else {
            print("Order", row["order"]!["id"]!, "by", "Unknown", "- $", row["order"]!["total"]!)
        }
    }
    else {
        print("Customer", row["customer"]!["name"]!, "has no orders")
    }
}
