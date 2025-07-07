struct Item { let n: Int; let v: String }
let items = [
    Item(n: 1, v: "a"),
    Item(n: 1, v: "b"),
    Item(n: 2, v: "c")
]
let result = items.sorted { $0.n < $1.n }
print(result.map { $0.v })
