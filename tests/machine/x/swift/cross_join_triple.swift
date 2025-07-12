var nums = [1, 2]
var letters = ["A", "B"]
var bools = [true, false]
var combos = ({
	var _res: [[String:Any]] = []
	for n in nums {
		for l in letters {
			for b in bools {
				_res.append(["n": n, "l": l, "b": b])
			}
		}
	}
	return _res
}())
print("--- Cross Join of three lists ---")
for c in combos as! [[String:Any]] {
    print(c["n"]!, c["l"]!, c["b"]!)
}
