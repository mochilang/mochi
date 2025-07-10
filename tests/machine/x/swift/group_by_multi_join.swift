var nations = [["id": 1, "name": "A"], ["id": 2, "name": "B"]]
var suppliers = [["id": 1, "nation": 1], ["id": 2, "nation": 2]]
var partsupp = [["part": 100, "supplier": 1, "cost": 10, "qty": 2], ["part": 100, "supplier": 2, "cost": 20, "qty": 1], ["part": 200, "supplier": 1, "cost": 5, "qty": 3]]
var filtered = ({
	var _res: [[String:Any]] = []
	for ps in partsupp {
		for s in suppliers {
			if !(s["id"] as! Int == ps["supplier"] as! Int) { continue }
			for n in nations {
				if !(n["id"] as! Int == s["nation"] as! Int) { continue }
				if !(n["name"] as! String == "A") { continue }
				_res.append(["part": ps["part"] as! Int, "value": ps["cost"] as! Double * ps["qty"] as! Int])
			}
		}
	}
	return _res
}())
var grouped = { () -> [Any] in
    let _groups = Dictionary(grouping: filtered) { x in x["part"] }
    var _tmp = _groups.map { (k, v) in (key: k, items: v) }
    return _tmp.map { g in (part: g.key, total: g.items.map { r in r["value"] }.reduce(0, +)) }
}()
print(grouped)
