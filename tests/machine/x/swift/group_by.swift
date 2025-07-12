struct AgeCity: Equatable {
    var age: Int
    var city: String
    var name: String
}

var people = [AgeCity(age: 30, city: "Paris", name: "Alice"), AgeCity(age: 15, city: "Hanoi", name: "Bob"), AgeCity(age: 65, city: "Paris", name: "Charlie"), AgeCity(age: 45, city: "Hanoi", name: "Diana"), AgeCity(age: 70, city: "Paris", name: "Eve"), AgeCity(age: 22, city: "Hanoi", name: "Frank")]
var stats = { () -> [Any] in
    var _groups: [String:[AgeCity]] = [:]
    for person in people {
        let _k = person.city
        _groups[_k, default: []].append(person)
    }
    var _tmp: [(key: String, items: [AgeCity])] = []
    for (k, v) in _groups {
        _tmp.append((key: k, items: v))
    }
    return _tmp.map { g in ["city": g.key, "count": g.items.count, "avg_age": (g.items.map { p in p.age }.reduce(0, +) / g.items.map { p in p.age }.count)] }
}()
print("--- People grouped by city ---")
for s in stats as! [[String:Any]] {
    print(s["city"]!, ": count =", s["count"]!, ", avg_age =", s["avg_age"]!)
}
