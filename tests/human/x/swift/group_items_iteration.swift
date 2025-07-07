struct Item { let tag: String; let val: Int }

let data = [
    Item(tag: "a", val: 1),
    Item(tag: "a", val: 2),
    Item(tag: "b", val: 3)
]

var groups: [String: [Item]] = [:]
for d in data {
    groups[d.tag, default: []].append(d)
}

var tmp: [(tag: String, total: Int)] = []
for (tag, items) in groups {
    let total = items.reduce(0) { $0 + $1.val }
    tmp.append((tag: tag, total: total))
}

tmp.sort { $0.tag < $1.tag }
print(tmp)
