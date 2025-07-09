let xs = [1, 2, 3]
let ys = xs.compactMap { x in x % 2 == 1 ? (x) : nil }
print(ys.contains(1))
print(ys.contains(2))
let m = ["a": 1]
print(m.keys.contains("a"))
print(m.keys.contains("b"))
let s = "hello"
print(s.contains("ell"))
print(s.contains("foo"))
