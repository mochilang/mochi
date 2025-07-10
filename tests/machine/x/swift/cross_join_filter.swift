var nums = [1, 2, 3]
var letters = ["A", "B"]
var pairs = nums.flatMap { n in letters.compactMap { l in n % 2 == 0 ? (["n": n, "l": l]) : nil } }
print("--- Even pairs ---")
for p in pairs as! [[String:Any]] {
    print(p["n"]!, p["l"]!)
}
