var customers = [["id": 1, "name": "Alice"], ["id": 2, "name": "Bob"], ["id": 3, "name": "Charlie"]]
var orders = [["id": 100, "customerId": 1], ["id": 101, "customerId": 1], ["id": 102, "customerId": 2]]
var stats = ({
	var _groups: [AnyHashable:[Any]] = [:]
	for c in customers {
		var _m = false
		for o in orders {
			if !(o.customerId == c.id) { continue }
			_m = true
			let _k = c["name"] as! String
			_groups[_k, default: []].append((c: c, o: o))
		}
		if !_m {
			let o: Any? = nil
			let _k = c["name"] as! String
			_groups[_k, default: []].append((c: c, o: o))
		}
	}
	var _tmp = _groups.map { (k, v) in (key: k, items: v) }
	return _tmp.map { g in (name: g.key, count: g.items.filter { r in r["o"] }.count) }
}())
print("--- Group Left Join ---")
for s in stats {
    print(s["name"], "orders:", s["count"])
}
