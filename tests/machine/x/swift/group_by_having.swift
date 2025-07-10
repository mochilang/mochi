import Foundation

func _json(_ v: Any) {
    func _sort(_ x: Any) -> Any {
        if let a = x as? [Any] { return a.map { _sort($0) } }
        if let m = x as? [String:Any] {
            var out: [String:Any] = [:]
            for k in m.keys.sorted() { out[k] = _sort(m[k]!) }
            return out
        }
        return x
    }
    if let obj = _sort(v) as? Any,
       let data = try? JSONSerialization.data(withJSONObject: obj),
       let s = String(data: data, encoding: .utf8) {
        print(s)
    }
}
var people = [["name": "Alice", "city": "Paris"], ["name": "Bob", "city": "Hanoi"], ["name": "Charlie", "city": "Paris"], ["name": "Diana", "city": "Hanoi"], ["name": "Eve", "city": "Paris"], ["name": "Frank", "city": "Hanoi"], ["name": "George", "city": "Paris"]]
var big = { () -> [Any] in
    var _groups: [AnyHashable:[[String:Any]]] = [:]
    for p in people {
        let _k = p["city"] as! String
        _groups[_k, default: []].append(p)
    }
    var _tmp: [(key: AnyHashable, items: [[String:Any]])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp = _tmp.filter { g in g.items.count >= 4 }
    return _tmp.map { g in (city: g.key, num: g.items.count) }
}()
_json(big)
