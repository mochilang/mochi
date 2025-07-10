var customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"]]
var orders = [["id": 100, "customerId": 1], ["id": 101, "customerId": 1], ["id": 102, "customerId": 2]]
var stats = ({
	var _groups: [AnyHashable:[Any]] = [:]
	for o in orders {
		for c in customers {
			if !(o["customerId"] as! Int == c["id"] as! Int) { continue }
			let _k = c["name"] as! String
			_groups[_k, default: []].append(["o": o, "c": c])
		}
	}
	var _tmp = _groups.map { (k, v) in (key: k, items: v) }
	return _tmp.map { g in ["name": g.key, "count": g.items.count] }
}())
print("--- Orders per customer ---")
for s in stats as! [[String:Any]] {
    print(s["name"]!, "orders:", s["count"]!)
}
