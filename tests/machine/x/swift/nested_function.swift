func outer(_ x: Int) -> Int {
    func inner(_ y: Int) -> Int {
        return x + y
    }
    return inner(5)
}
print(outer(3))
