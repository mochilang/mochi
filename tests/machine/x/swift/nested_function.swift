func outer(x: Int) -> Int {
    func inner(y: Int) -> Int {
        return x + y
    }
    return inner(5)
}
print(outer(3))
