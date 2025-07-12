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
struct CityName: Equatable {
    var city: String
    var name: String
}

var people = [CityName(city: "Paris", name: "Alice"), CityName(city: "Hanoi", name: "Bob"), CityName(city: "Paris", name: "Charlie"), CityName(city: "Hanoi", name: "Diana"), CityName(city: "Paris", name: "Eve"), CityName(city: "Hanoi", name: "Frank"), CityName(city: "Paris", name: "George")]
var big = { () -> [Any] in
    var _groups: [String:[CityName]] = [:]
    for p in people {
        let _k = p.city
        _groups[_k, default: []].append(p)
    }
    var _tmp: [(key: String, items: [CityName])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    _tmp = _tmp.filter { g in g.items.count >= 4 }
    return _tmp.map { g in ["city": g.key, "num": g.items.count] }
}()
_json(big)
