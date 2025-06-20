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
	
	let m = s.count
	let n = p.count
	var dp: [[Bool]] = []
	var i = 0
	while i <= m {
		var row: [Bool] = []
		var j = 0
		while j <= n {
			row = row + [false]
			j = j + 1
		}
		dp = dp + [row]
		i = i + 1
	}
	dp[m][n] = true
	var i2 = m
	while i2 >= 0 {
		var j2 = n - 1
		while j2 >= 0 {
			var first = false
			if i2 < m {
				if (_indexString(p, j2) == _indexString(s, i2)) || (_indexString(p, j2) == ".") {
					first = true
				}
			}
			var star = false
			if j2 + 1 < n {
				if _indexString(p, j2 + 1) == "*" {
					star = true
				}
			}
			if star {
				var ok = false
				if dp[i2][j2 + 2] {
					ok = true
				} else {
					if first {
						if dp[i2 + 1][j2] {
							ok = true
						}
					}
				}
				dp[i2][j2] = ok
			} else {
				var ok = false
				if first {
					if dp[i2 + 1][j2 + 1] {
						ok = true
					}
				}
				dp[i2][j2] = ok
			}
			j2 = j2 - 1
		}
		i2 = i2 - 1
	}
	return dp[0][0]
}

func main() {
}
main()
