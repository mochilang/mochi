nums = [1, 2, 3]
letters = ["A", "B"]
pairs = [{"n": n, "l": l} for n in nums for l in letters if n % 2 == 0]
print("--- Even pairs ---")
for p in pairs:
    print(p["n"], p["l"])
