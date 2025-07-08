struct Counter {
    var n: Int
}
func inc(_ c: inout Counter) {
    c.n = c.n + 1
}
var c = Counter(n: 0)
inc(&c)
print(c.n)
