import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
	var idx = i
	let chars = Array(s)
	if idx < 0 { idx += chars.count }
	if idx < 0 || idx >= chars.count { fatalError("index out of range") }
	return String(chars[idx])
}

func isMatch(_ s: String, _ p: String) -> Bool {
	let s = s
	let p = p
	
	let m: Int = s.count
	let n: Int = p.count
	var memo: [Int: Bool] = [:]
	func dfs(_ i: Int, _ j: Int) -> Bool {
		let i = i
		let j = j
		
		let key: Int = i * (n + 1) + j
		if memo[key] != nil {
			return memo[key]!
		}
		if j == n {
			return i == m
		}
		var first: Bool = false
		if i < m {
			if (_indexString(p, j) == _indexString(s, i)) || (_indexString(p, j) == ".") {
				first = true
			}
		}
		var ans: Bool = false
		if j + 1 < n {
			if _indexString(p, j + 1) == "*" {
				if dfs(i, j + 2) {
					ans = true
				} else 				if first && dfs(i + 1, j) {
					ans = true
				}
			} else {
				if first && dfs(i + 1, j + 1) {
					ans = true
				}
			}
		} else {
			if first && dfs(i + 1, j + 1) {
				ans = true
			}
		}
		memo[key] = ans
		return ans
	}
	return dfs(0, 0)
}

func main() {
}
main()
