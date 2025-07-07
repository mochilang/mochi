let nums = [1, 2, 3]
let letters = ["A", "B"]
var pairs: [(Int, String)] = []

for n in nums where n % 2 == 0 {
    for l in letters {
        pairs.append((n, l))
    }
}

print("--- Even pairs ---")
for p in pairs {
    print(p.0, p.1)
}
