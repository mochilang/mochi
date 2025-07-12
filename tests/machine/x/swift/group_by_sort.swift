struct CatVal: Equatable {
    var cat: String
    var val: Int
}

var items = [CatVal(cat: "a", val: 3), CatVal(cat: "a", val: 1), CatVal(cat: "b", val: 5), CatVal(cat: "b", val: 2)]
var grouped = { () -> [Any] in
    var _groups: [String:[CatVal]] = [:]
    for i in items {
        let _k = i.cat
        _groups[_k, default: []].append(i)
    }
    var _tmp: [(key: String, items: [CatVal])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp.sort { String(describing: $0.items.map { x in x.val }.reduce(0, +)) > String(describing: $1.items.map { x in x.val }.reduce(0, +)) }
    return _tmp.map { g in ["cat": g.key, "total": g.items.map { x in x.val }.reduce(0, +)] }
}()
print(grouped)
