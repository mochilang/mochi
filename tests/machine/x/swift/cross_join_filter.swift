var nums = [1, 2, 3]
var letters = ["A", "B"]
var pairs = ({
	var _res: [[String:Any]] = []
	for n in nums {
		for l in letters {
			if !(n % 2 == 0) { continue }
			_res.append(["n": n, "l": l])
		}
	}
	return _res
}())
print("--- Even pairs ---")
for p in pairs as! [[String:Any]] {
    print(p["n"]!, p["l"]!)
}
