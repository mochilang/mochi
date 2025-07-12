struct Auto1: Equatable {
    var tag: String
    var val: Int
}

struct Group: Equatable {
    var items: [Auto1]
    var key: String
}

var data = [Auto1(tag: "a", val: 1), Auto1(tag: "a", val: 2), Auto1(tag: "b", val: 3)]
var groups = { () -> [(key: String, items: [Auto1])] in
    var _groups: [String:[Auto1]] = [:]
    for d in data {
        let _k = d.tag
        _groups[_k, default: []].append(d)
    }
    var _tmp: [(key: String, items: [Auto1])] = []
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
