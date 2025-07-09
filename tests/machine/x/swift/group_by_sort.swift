let items = [["cat": "a", "val": 3], ["cat": "a", "val": 1], ["cat": "b", "val": 5], ["cat": "b", "val": 2]]
let grouped = { () -> [Any] in
    let _groups = Dictionary(grouping: items) { i in i["cat"] as! String }
    var _tmp = _groups.map { (k, v) in (key: k, items: v) }
    _tmp.sort { $0.items.map { x in x.val }.reduce(0, +) > $1.items.map { x in x.val }.reduce(0, +) }
    return _tmp.map { g in ["cat": g.key, "total": g.items.map { x in x.val }.reduce(0, +)] }
}()
print(grouped)
