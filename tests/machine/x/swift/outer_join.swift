var customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"], ["id": 3, "name": "Charlie"], ["id": 4, "name": "Diana"]]
var orders = [["id": 100, "customerId": 1, "total": 250], ["id": 101, "customerId": 2, "total": 125], ["id": 102, "customerId": 1, "total": 300], ["id": 103, "customerId": 5, "total": 80]]
var result = ({
	var _res: [[String:Any]] = []
	let _src = orders
	let _join = customers
	var _matched = Array(repeating: false, count: _join.count)
	for o in _src {
		var _m = false
		for (ri, c) in _join.enumerated() {
			if !(o["customerId"] as! Int == c["id"] as! Int) { continue }
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
for row in result {
    if row["order"] != nil {
        if row["customer"] != nil {
            print("Order", row["order"]["id"], "by", row["customer"]["name"], "- $", row["order"]["total"])
        }
        else {
            print("Order", row["order"]["id"], "by", "Unknown", "- $", row["order"]["total"])
        }
    }
    else {
        print("Customer", row["customer"]["name"], "has no orders")
    }
}
