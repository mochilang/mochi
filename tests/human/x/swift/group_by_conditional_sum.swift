struct Item { let cat: String; let val: Int; let flag: Bool }

let items = [
    Item(cat: "a", val: 10, flag: true),
    Item(cat: "a", val: 5, flag: false),
    Item(cat: "b", val: 20, flag: true)
]

var groups: [String: [Item]] = [:]
for it in items {
    groups[it.cat, default: []].append(it)
}

var result: [(cat: String, share: Double)] = []
for key in groups.keys.sorted() {
    let g = groups[key]!
    let total = g.reduce(0) { $0 + $1.val }
    let flagged = g.reduce(0) { $0 + ($1.flag ? $1.val : 0) }
    result.append((cat: key, share: Double(flagged) / Double(total)))
}

print(result)
