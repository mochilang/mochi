import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func myAtoi(_ s: String) -> Int {
	let s = s
	
	var i: Int = 0
	let n: Int = s.count
	while i < n && _indexString(s, i) == " " {
		i = i + 1
	}
	var sign: Int = 1
	if i < n && (_indexString(s, i) == "+" || _indexString(s, i) == "-") {
		if _indexString(s, i) == "-" {
			sign = -1
		}
		i = i + 1
	}
	let digits: [String: Int] = ["0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9]
	var result: Int = 0
	while i < n {
		let ch: String = _indexString(s, i)
		if !(digits[ch] != nil) {
			break
		}
		let d: Int = digits[ch]!
		result = result * 10 + d
		i = i + 1
	}
	result = result * sign
	if result > 2147483647 {
		return 2147483647
	}
	if result < (-2147483648) {
		return -2147483648
	}
	return result
}

func main() {
}
main()
