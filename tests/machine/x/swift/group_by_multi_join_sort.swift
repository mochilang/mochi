import Foundation

class _Group {
    var key: Any
    var Items: [Any] = []
    init(_ k: Any) { self.key = k }
}

func _keyStr(_ v: Any) -> String {
    if let data = try? JSONSerialization.data(withJSONObject: v, options: [.sortedKeys]),
       let s = String(data: data, encoding: .utf8) {
        return s
    }
    return String(describing: v)
}
struct Auto1: Equatable {
    var n_name: String
    var n_nationkey: Int
}

struct Auto2: Equatable {
    var c_acctbal: Double
    var c_address: String
    var c_comment: String
    var c_custkey: Int
    var c_name: String
    var c_nationkey: Int
    var c_phone: String
}

struct Auto3: Equatable {
    var o_custkey: Int
    var o_orderdate: String
    var o_orderkey: Int
}

struct Auto4: Equatable {
    var l_discount: Double
    var l_extendedprice: Double
    var l_orderkey: Int
    var l_returnflag: String
}

var nation = [Auto1(n_name: "BRAZIL", n_nationkey: 1)]
var customer = [Auto2(c_acctbal: 100.0, c_address: "123 St", c_comment: "Loyal", c_custkey: 1, c_name: "Alice", c_nationkey: 1, c_phone: "123-456")]
var orders = [Auto3(o_custkey: 1, o_orderdate: "1993-10-15", o_orderkey: 1000), Auto3(o_custkey: 1, o_orderdate: "1994-01-02", o_orderkey: 2000)]
var lineitem = [Auto4(l_discount: 0.1, l_extendedprice: 1000.0, l_orderkey: 1000, l_returnflag: "R"), Auto4(l_discount: 0.0, l_extendedprice: 500.0, l_orderkey: 2000, l_returnflag: "N")]
let start_date = "1993-10-01"
let end_date = "1994-01-01"
var result = { () -> [Any] in
	var _groups: [String:_Group] = [:]
	var _order: [String] = []
	for c in customer {
		for o in orders {
			if !(o.o_custkey == c.c_custkey) { continue }
			for l in lineitem {
				if !(l.l_orderkey == o.o_orderkey) { continue }
				for n in nation {
					if !(n.n_nationkey == c.c_nationkey) { continue }
					if !(o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R") { continue }
					let _k = ["c_custkey": c.c_custkey, "c_name": c.c_name, "c_acctbal": c.c_acctbal, "c_address": c.c_address, "c_phone": c.c_phone, "c_comment": c.c_comment, "n_name": n.n_name]
					let _ks = _keyStr(_k)
					if _groups[_ks] == nil {
					    _groups[_ks] = _Group(_k)
					    _order.append(_ks)
					}
					_groups[_ks]!.Items.append(["c": c, "o": o, "l": l, "n": n])
				}
			}
		}
	}
	var _tmp: [(key: [String:Any], items: [[String:Any]])] = []
	for k in _order {
	    if let g = _groups[k] {
	        _tmp.append((key: g.key as! [String:Any], items: g.Items.map { $0 as! [String:Any] }))
	    }
	}
	_tmp.sort { $0.items.map { x in x["l"] as! Auto4["l_extendedprice"]! * (1 - x["l"] as! Auto4["l_discount"]!) }.reduce(0, +) > $1.items.map { x in x["l"] as! Auto4["l_extendedprice"]! * (1 - x["l"] as! Auto4["l_discount"]!) }.reduce(0, +) }
	return _tmp.map { g in ["c_custkey": g.key.c_custkey, "c_name": g.key.c_name, "revenue": g.items.map { x in x["l"] as! Auto4["l_extendedprice"]! * (1 - x["l"] as! Auto4["l_discount"]!) }.reduce(0, +), "c_acctbal": g.key.c_acctbal, "n_name": g.key.n_name, "c_address": g.key.c_address, "c_phone": g.key.c_phone, "c_comment": g.key.c_comment] }
}())
print(result)
