var data = [["tag": "a", "val": 1], ["tag": "a", "val": 2], ["tag": "b", "val": 3]]
var groups = { () -> [(key: AnyHashable, items: [[String:Any]])] in
    var _groups: [AnyHashable:[[String:Any]]] = [:]
    for d in data {
        let _k = d["tag"] as! String
        _groups[_k, default: []].append(d)
    }
    var _tmp: [(key: AnyHashable, items: [[String:Any]])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    return _tmp
}()
var tmp = []
for g in groups {
    var total = 0
    for x in g.items {
        total = total + x.val
    }
    tmp = tmp + [["tag": g.key, "total": total]]
}
var result = tmp.map { r in (value: r, key: r.tag) }.sorted { $0.key < $1.key }.map { $0.value }
print(result)
