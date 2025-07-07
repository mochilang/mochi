struct Item { let a: Int; let b: Int }
let data = [
    Item(a: 1, b: 2),
    Item(a: 1, b: 1),
    Item(a: 0, b: 5)
]
let sorted = data.sorted { lhs, rhs in
    if lhs.a == rhs.a { return lhs.b < rhs.b }
    return lhs.a < rhs.a
}
for s in sorted {
    print("{a:\(s.a), b:\(s.b)}")
}
