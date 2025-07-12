struct Group: Equatable {
    var items: [TagVal]
    var key: String
}

struct TagVal: Equatable {
    var tag: String
    var val: Int
}

var data = [TagVal(tag: "a", val: 1), TagVal(tag: "a", val: 2), TagVal(tag: "b", val: 3)]
var groups = { () -> [(key: String, items: [TagVal])] in
    var _groups: [String:[TagVal]] = [:]
    for d in data {
        let _k = d.tag
        _groups[_k, default: []].append(d)
    }
    var _tmp: [(key: String, items: [TagVal])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    return _tmp
}()
var tmp = [Any]()
for g in groups {
    var total = 0
    for x in g.items {
        total = total + x.val
    }
    tmp = tmp + [["tag": g.key, "total": total]]
}
var result = tmp.map { r in (value: r, key: r["tag"] as! String) }.sorted { $0.key < $1.key }.map { $0.value }
print(result)
