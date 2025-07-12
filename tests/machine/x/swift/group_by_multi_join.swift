struct CostPart: Equatable {
    var cost: Double
    var part: Int
    var qty: Int
    var supplier: Int
}

struct IdName: Equatable {
    var id: Int
    var name: String
}

struct IdNation: Equatable {
    var id: Int
    var nation: Int
}

var nations = [IdName(id: 1, name: "A"), IdName(id: 2, name: "B")]
var suppliers = [IdNation(id: 1, nation: 1), IdNation(id: 2, nation: 2)]
var partsupp = [CostPart(cost: 10.0, part: 100, qty: 2, supplier: 1), CostPart(cost: 20.0, part: 100, qty: 1, supplier: 2), CostPart(cost: 5.0, part: 200, qty: 3, supplier: 1)]
var filtered = ({
	var _res: [[String:Any]] = []
	for ps in partsupp {
		for s in suppliers {
			if !(s.id == ps.supplier) { continue }
			for n in nations {
				if !(n.id == s.nation) { continue }
				if !(n.name == "A") { continue }
				_res.append(["part": ps.part, "value": ps.cost * ps.qty])
			}
		}
	}
	return _res
}())
var grouped = { () -> [Any] in
    var _groups: [Any:[[String:Any]]] = [:]
    for x in filtered {
        let _k = x["part"]!
        _groups[_k, default: []].append(x)
    }
    var _tmp: [(key: Any, items: [[String:Any]])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    return _tmp.map { g in ["part": g.key, "total": g.items.map { r in r["value"]! }.reduce(0, +)] }
}()
print(grouped)
