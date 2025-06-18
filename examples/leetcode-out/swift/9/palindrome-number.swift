import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func isPalindrome(_ x: Int) -> Bool {
	let x = x
	
	if x < 0 {
		return false
	}
	let s: String = String(x)
	let n: Int = s.count
	for i in 0..<n / 2 {
		if _indexString(s, i) != _indexString(s, n - 1 - i) {
			return false
		}
	}
	return true
}

func main() {
}
main()
