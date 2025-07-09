let people = [["name": "Alice", "city": "Paris"], ["name": "Bob", "city": "Hanoi"], ["name": "Charlie", "city": "Paris"], ["name": "Diana", "city": "Hanoi"], ["name": "Eve", "city": "Paris"], ["name": "Frank", "city": "Hanoi"], ["name": "George", "city": "Paris"]]
let big = { () -> [Any] in
    let _groups = Dictionary(grouping: people) { p in p["city"] as! String }
    var _tmp = _groups.map { (k, v) in (key: k, items: v) }
    _tmp = _tmp.filter { g in g.items.count >= 4 }
    return _tmp.map { g in ["city": g.key, "num": g.items.count] }
}()
json(big)
