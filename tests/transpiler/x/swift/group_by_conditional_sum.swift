// Generated by Mochi transpiler v0.10.33 on 2025-07-21 16:41:05 GMT+7
let items = [["cat": "a", "val": 10, "flag": true], ["cat": "a", "val": 5, "flag": false], ["cat": "b", "val": 20, "flag": true]]
let result = ({ var _groups: [String: [String: Any]] = [:]
var _res: [[String: Any]] = []
for i in items {
    let _key = i["cat"]!
    let _ks = String(describing: _key)
    var _g = _groups[_ks] ?? ["key": _key, "items": []]
    var _item: [String: Any] = ["__join__": true]
    _item["i"] = i
    _g["items"] = (_g["items"] as! [Any]) + [_item]
    _groups[_ks] = _g
}
var _list = Array(_groups.values)
_list.sort { a, b in
let g = a
let _ka = g["key"]!
let g = b
let _kb = g["key"]!
return String(describing: _ka) < String(describing: _kb)
}
for g in _list {
    _res.append(["cat": g["key"]!, "share": (((({ var _res: [Any] = []
    for x in g["items"] as! [[String: Any]] {
        _res.append((x["flag"]! ? x["val"]! : 0))
    }
    return _res })().reduce(0,+)) as! Double) / ((({ var _res: [Any] = []
    for x in g["items"] as! [[String: Any]] {
        _res.append(x["val"]!)
    }
    return _res })().reduce(0,+)) as! Double))])
}
return _res })()
print(result)
