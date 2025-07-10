var data = [["tag": "a", "val": 1], ["tag": "a", "val": 2], ["tag": "b", "val": 3]]
var groups = { () -> [Any] in
    let _groups = Dictionary(grouping: data) { d in d["tag"] as! String }
    var _tmp = _groups.map { (k, v) in (key: k, items: v) }
    return _tmp.map { g in g }
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
