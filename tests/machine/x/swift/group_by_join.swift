// Generated by Mochi compiler v0.10.27 on 2006-01-02T15:04:05Z
struct Customer: Equatable {
    var id: Int
    var name: String
}

struct Order: Equatable {
    var customerId: Int
    var id: Int
}

var customers = [Customer(id: 1, name: "Alice"), Customer(id: 2, name: "Bob")]
var orders = [Order(customerId: 1, id: 100), Order(customerId: 1, id: 101), Order(customerId: 2, id: 102)]
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
}()
print("--- Orders per customer ---")
for s in stats as! [[String:Any]] {
    print(s["name"]!, "orders:", s["count"]!)
}
