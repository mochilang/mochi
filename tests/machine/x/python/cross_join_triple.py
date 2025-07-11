nums = [1, 2]
letters = ["A", "B"]
bools = [True, False]
combos = [{"n": n, "l": l, "b": b} for n in nums for l in letters for b in bools]
print("--- Cross Join of three lists ---")
for c in combos:
    print(c["n"], c["l"], c["b"])
