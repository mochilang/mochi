struct Item { let cat: String; let val: Int }

let items = [
    Item(cat: "a", val: 3),
    Item(cat: "a", val: 1),
    Item(cat: "b", val: 5),
    Item(cat: "b", val: 2)
]

var totals: [String: Int] = [:]
for i in items {
    totals[i.cat, default: 0] += i.val
}

var grouped = totals.map { (cat: $0.key, total: $0.value) }
grouped.sort { $0.total > $1.total }

print(grouped)
