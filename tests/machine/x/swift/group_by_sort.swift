var items = [["cat": "a", "val": 3], ["cat": "a", "val": 1], ["cat": "b", "val": 5], ["cat": "b", "val": 2]]
var grouped = { () -> [Any] in
    var _groups: [AnyHashable:[[String:Any]]] = [:]
    for i in items {
        let _k = i["cat"] as! String
        _groups[_k, default: []].append(i)
    }
    var _tmp: [(key: AnyHashable, items: [[String:Any]])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp.sort { $0.items.map { x in x["val"] }.reduce(0, +) > $1.items.map { x in x["val"] }.reduce(0, +) }
    return _tmp.map { g in (cat: g.key, total: g.items.map { x in x["val"] }.reduce(0, +)) }
}()
print(grouped)
