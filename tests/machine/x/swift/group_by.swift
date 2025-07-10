var people = [["name": "Alice", "age": 30, "city": "Paris"], ["name": "Bob", "age": 15, "city": "Hanoi"], ["name": "Charlie", "age": 65, "city": "Paris"], ["name": "Diana", "age": 45, "city": "Hanoi"], ["name": "Eve", "age": 70, "city": "Paris"], ["name": "Frank", "age": 22, "city": "Hanoi"]]
var stats = { () -> [Any] in
    var _groups: [AnyHashable:[[String:Any]]] = [:]
    for person in people {
        let _k = person["city"] as! String
        _groups[_k, default: []].append(person)
    }
    var _tmp: [(key: AnyHashable, items: [[String:Any]])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    return _tmp.map { g in ["city": g.key, "count": g.items.count, "avg_age": (g.items.map { p in p["age"] as! Int }.reduce(0, +) / g.items.map { p in p["age"] as! Int }.count)] }
}()
print("--- People grouped by city ---")
for s in stats as! [[String:Any]] {
    print(s["city"]!, ": count =", s["count"]!, ", avg_age =", s["avg_age"]!)
}
