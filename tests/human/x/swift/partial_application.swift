func add(_ a: Int, _ b: Int) -> Int {
    return a + b
}
let add5: (Int) -> Int = { b in add(5, b) }
print(add5(3))
