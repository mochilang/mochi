let nums = [1, 2]
let letters = ["A", "B"]
let bools = [true, false]
let combos = nums.flatMap { n in letters.flatMap { l in bools.map { b in (n: n, l: l, b: b) } } }
print("--- Cross Join of three lists ---")
for c in combos {
    print(c.n, c.l, c.b)
}
