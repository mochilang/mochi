import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func _slice<T>(_ arr: [T], _ i: Int, _ j: Int) -> [T] {
	var start = i
	var end = j
	let n = arr.count
	if start < 0 { start += n }
	if end < 0 { end += n }
	if start < 0 { start = 0 }
	if end > n { end = n }
	if end < start { end = start }
	return Array(arr[start..<end])
}

func isValid(_ s: String) -> Bool {
	let s = s
	
	var stack: [String] = []
	let n = s.count
	for i in 0..<n {
		let c = _indexString(s, i)
		if c == "(" {
			stack = stack + [")"]
		} else 		if c == "[" {
			stack = stack + ["]"]
		} else 		if c == "{" {
			stack = stack + ["}"]
		} else {
			if stack.count == 0 {
				return false
			}
			let top = stack[stack.count - 1]
			if top != c {
				return false
			}
			stack = _slice(stack, 0, stack.count - 1)
		}
	}
	return stack.count == 0
}

func main() {
}
main()
