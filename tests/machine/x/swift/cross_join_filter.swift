let nums = [1, 2, 3]
let letters = ["A", "B"]
let pairs = nums.flatMap { n in letters.compactMap { l in n % 2 == 0 ? ((n: n, l: l)) : nil } }
print("--- Even pairs ---")
for p in pairs {
    print(p.n, p.l)
}
