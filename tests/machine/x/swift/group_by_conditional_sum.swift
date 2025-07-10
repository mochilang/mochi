var items = [["cat": "a", "val": 10, "flag": true], ["cat": "a", "val": 5, "flag": false], ["cat": "b", "val": 20, "flag": true]]
var result = { () -> [Any] in
    let _groups = Dictionary(grouping: items) { i in i["cat"] as! String }
    var _tmp = _groups.map { (k, v) in (key: k, items: v) }
    _tmp.sort { $0.key < $1.key }
    return _tmp.map { g in (cat: g.key, share: g.items.map { x in x["flag"] ? x["val"] : 0 }.reduce(0, +) / g.items.map { x in x["val"] }.reduce(0, +)) }
}()
print(result)
