import Foundation

func letterCombinations(_ digits: String) -> [String] {
	let digits = digits
	
	if digits.count == 0 {
		return []
	}
	let mapping = ["2": ["a", "b", "c"], "3": ["d", "e", "f"], "4": ["g", "h", "i"], "5": ["j", "k", "l"], "6": ["m", "n", "o"], "7": ["p", "q", "r", "s"], "8": ["t", "u", "v"], "9": ["w", "x", "y", "z"]]
	var result = [""]
	for d_ch in digits {
		let d = String(d_ch)
		if !(mapping[d] != nil) {
			continue
		}
		let letters = mapping[d]!
		let next = ({
	var _res: [String] = []
	for p in result {
		for ch in letters {
			_res.append(p + ch)
		}
	}
	return _res
}())
		result = next
	}
	return result
}

func main() {
}
main()
