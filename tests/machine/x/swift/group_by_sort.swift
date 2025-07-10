struct Auto1: Equatable {
    var cat: String
    var val: Int
}

var items = [Auto1(cat: "a", val: 3), Auto1(cat: "a", val: 1), Auto1(cat: "b", val: 5), Auto1(cat: "b", val: 2)]
var grouped = { () -> [Any] in
    var _groups: [String:[Auto1]] = [:]
    for i in items {
        let _k = i.cat
        _groups[_k, default: []].append(i)
    }
    var _tmp: [(key: String, items: [Auto1])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp.sort { String(describing: $0.items.map { x in x.val }.reduce(0, +)) > String(describing: $1.items.map { x in x.val }.reduce(0, +)) }
    return _tmp.map { g in ["cat": g.key, "total": g.items.map { x in x.val }.reduce(0, +)] }
}()
print(grouped)
