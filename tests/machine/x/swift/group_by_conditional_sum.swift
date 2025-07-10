var items = [["cat": "a", "val": 10, "flag": true], ["cat": "a", "val": 5, "flag": false], ["cat": "b", "val": 20, "flag": true]]
var result = { () -> [Any] in
    var _groups: [AnyHashable:[[String:Any]]] = [:]
    for i in items {
        let _k = i["cat"] as! String
        _groups[_k, default: []].append(i)
    }
    var _tmp: [(key: AnyHashable, items: [[String:Any]])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp.sort { $0.key < $1.key }
    return _tmp.map { g in (cat: g.key, share: g.items.map { x in x["flag"] ? x["val"] : 0 }.reduce(0, +) / g.items.map { x in x["val"] }.reduce(0, +)) }
}()
print(result)
