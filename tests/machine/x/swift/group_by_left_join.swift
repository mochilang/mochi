struct CustomerIdId: Equatable {
    var customerId: Int
    var id: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

var customers = [IdName(id: 1, name: "Alice"), IdName(id: 2, name: "Bob"), IdName(id: 3, name: "Charlie")]
var orders = [CustomerIdId(customerId: 1, id: 100), CustomerIdId(customerId: 1, id: 101), CustomerIdId(customerId: 2, id: 102)]
var stats = { () -> [Any] in
	var _groups: [String:[[String:Any]]] = [:]
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
	var _tmp: [(key: String, items: [[String:Any]])] = []
	for (k, v) in _groups {
	    _tmp.append((key: k, items: v))
	}
	return _tmp.map { g in ["name": g.key, "count": g.items.filter { r in r["o"] as! CustomerIdId != nil }.count] }
}())
print("--- Group Left Join ---")
for s in stats as! [[String:Any]] {
    print(s["name"]!, "orders:", s["count"]!)
}
