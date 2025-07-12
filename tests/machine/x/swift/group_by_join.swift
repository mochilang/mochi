struct CustomerIdId: Equatable {
    var customerId: Int
    var id: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

var customers = [IdName(id: 1, name: "Alice"), IdName(id: 2, name: "Bob")]
var orders = [CustomerIdId(customerId: 1, id: 100), CustomerIdId(customerId: 1, id: 101), CustomerIdId(customerId: 2, id: 102)]
var stats = { () -> [Any] in
	var _groups: [String:[[String:Any]]] = [:]
	for o in orders {
		for c in customers {
			if !(o.customerId == c.id) { continue }
			let _k = c.name
			_groups[_k, default: []].append(["o": o, "c": c])
		}
	}
	var _tmp: [(key: String, items: [[String:Any]])] = []
	for (k, v) in _groups {
	    _tmp.append((key: k, items: v))
	}
	return _tmp.map { g in ["name": g.key, "count": g.items.count] }
}())
print("--- Orders per customer ---")
for s in stats as! [[String:Any]] {
    print(s["name"]!, "orders:", s["count"]!)
}
