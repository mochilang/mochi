func add(_ a: Int, _ b: Int) -> Int {
    return a + b
}
let add5 = { (_ p0: Int) in add(5, p0) }
print(add5(3))
