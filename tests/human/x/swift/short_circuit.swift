func boom(_ a: Int, _ b: Int) -> Bool {
    print("boom")
    return true
}
print(false && boom(1, 2))
print(true || boom(1, 2))
