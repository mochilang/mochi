var people = [["name": "Alice", "age": 30, "city": "Paris"], ["name": "Bob", "age": 15, "city": "Hanoi"], ["name": "Charlie", "age": 65, "city": "Paris"], ["name": "Diana", "age": 45, "city": "Hanoi"], ["name": "Eve", "age": 70, "city": "Paris"], ["name": "Frank", "age": 22, "city": "Hanoi"]]
var stats = { () -> [Any] in
    let _groups = Dictionary(grouping: people) { person in person["city"] as! String }
    var _tmp = _groups.map { (k, v) in (key: k, items: v) }
    return _tmp.map { g in (city: g.key, count: g.items.count, avg_age: (g.items.map { p in p["age"] }.reduce(0, +) / g.items.map { p in p["age"] }.count)) }
}()
print("--- People grouped by city ---")
for s in stats {
    print(s["city"], ": count =", s["count"], ", avg_age =", s["avg_age"])
}
