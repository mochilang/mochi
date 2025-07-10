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
var orders = [Auto2(customerId: 1, id: 100, total: 250), Auto2(customerId: 2, id: 101, total: 125), Auto2(customerId: 1, id: 102, total: 300), Auto2(customerId: 5, id: 103, total: 80)]
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
