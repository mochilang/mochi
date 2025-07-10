struct Auto1: Equatable {
    var id: Int
    var name: String
}

struct Auto2: Equatable {
    var customerId: Int
    var id: Int
}

var customers = [Auto1(id: 1, name: "Alice"), Auto1(id: 2, name: "Bob"), Auto1(id: 3, name: "Charlie")]
var orders = [Auto2(customerId: 1, id: 100), Auto2(customerId: 1, id: 101), Auto2(customerId: 2, id: 102)]
var stats = ({
	var _groups: [AnyHashable:[Any]] = [:]
	for c in customers {
		var _m = false
		for o in orders {
			if !(o.customerId == c.id) { continue }
			_m = true
			let _k = c.name
			_groups[_k, default: []].append(["c": c, "o": o])
		}
		if !_m {
			let o: Any? = nil
			let _k = c.name
			_groups[_k, default: []].append(["c": c, "o": o])
		}
	}
	var _tmp = _groups.map { (k, v) in (key: k, items: v) }
	return _tmp.map { g in ["name": g.key, "count": g.items.filter { r in r["o"] as! Auto2 != nil }.count] }
}())
print("--- Group Left Join ---")
for s in stats as! [[String:Any]] {
    print(s["name"]!, "orders:", s["count"]!)
}
