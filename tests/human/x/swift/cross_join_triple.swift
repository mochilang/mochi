let nums = [1, 2]
let letters = ["A", "B"]
let bools = [true, false]
var combos: [(Int, String, Bool)] = []

for n in nums {
    for l in letters {
        for b in bools {
            combos.append((n, l, b))
        }
    }
}

print("--- Cross Join of three lists ---")
for c in combos {
    print(c.0, c.1, c.2)
}
