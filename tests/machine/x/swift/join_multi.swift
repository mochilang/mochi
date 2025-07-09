let customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"]]
let orders = [["id": 100, "customerId": 1], ["id": 101, "customerId": 2]]
let items = [["orderId": 100, "sku": "a"], ["orderId": 101, "sku": "b"]]
let result = ({
	var _res: [[String:Any]] = []
	for o in orders {
		for c in customers {
			if !(o["customerId"] as! Int == c["id"] as! Int) { continue }
			for i in items {
				if !(o["id"] as! Int == i["orderId"] as! Int) { continue }
				_res.append(["name": c["name"] as! String, "sku": i["sku"] as! String])
			}
		}
	}
	return _res
}())
print("--- Multi Join ---")
for r in result {
    print(r["name"], "bought item", r["sku"])
}
