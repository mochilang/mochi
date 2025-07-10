struct Auto1: Equatable {
    var id: Int
    var name: String
}

struct Auto2: Equatable {
    var customerId: Int
    var id: Int
}

var customers = [Auto1(id: 1, name: "Alice"), Auto1(id: 2, name: "Bob")]
var orders = [Auto2(customerId: 1, id: 100), Auto2(customerId: 1, id: 101), Auto2(customerId: 2, id: 102)]
var stats = ({
	var _groups: [AnyHashable:[Any]] = [:]
	for o in orders {
		for c in customers {
			if !(o.customerId == c.id) { continue }
			let _k = c.name
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
