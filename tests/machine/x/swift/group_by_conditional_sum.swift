struct CatFlag: Equatable {
    var cat: String
    var flag: Bool
    var val: Int
}

var items = [CatFlag(cat: "a", flag: true, val: 10), CatFlag(cat: "a", flag: false, val: 5), CatFlag(cat: "b", flag: true, val: 20)]
var result = { () -> [Any] in
    var _groups: [String:[CatFlag]] = [:]
    for i in items {
        let _k = i.cat
        _groups[_k, default: []].append(i)
    }
    var _tmp: [(key: String, items: [CatFlag])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp.sort { String(describing: $0.key) < String(describing: $1.key) }
    return _tmp.map { g in ["cat": g.key, "share": g.items.map { x in x.flag ? x.val : 0 }.reduce(0, +) / g.items.map { x in x.val }.reduce(0, +)] }
}()
print(result)
