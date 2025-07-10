var customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"], ["id": 3, "name": "Charlie"], ["id": 4, "name": "Diana"]]
var orders = [["id": 100, "customerId": 1, "total": 250], ["id": 101, "customerId": 2, "total": 125], ["id": 102, "customerId": 1, "total": 300]]
var result = ({
	var _res: [[String:Any]] = []
	let _src = customers
	let _join = orders
	for o in _join {
		var _m = false
		for c in _src {
			if !(o["customerId"] as! Int == c["id"] as! Int) { continue }
			_m = true
			_res.append(["customerName": c["name"] as! String, "order": o])
		}
		if !_m {
			let c: Any? = nil
			_res.append(["customerName": c["name"] as! String, "order": o])
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
