package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isMatch(s string, p string) bool {
	var m int = len(s)
	var n int = len(p)
	var memo map[int]bool = map[int]bool{}
	var dfs func(int, int) bool
	dfs = func(i int, j int) bool {
		var key int = ((i * ((n + 1))) + j)
		_tmp0 := key
		_tmp1 := memo
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			return memo[key]
		}
		if (j == n) {
			return (i == m)
		}
		var first bool = false
		if (i < m) {
			if (((_indexString(p, j) == _indexString(s, i))) || ((_indexString(p, j) == "."))) {
				first = true
			}
		}
		var ans bool = false
		if ((j + 1) < n) {
			if (_indexString(p, (j + 1)) == "*") {
				if dfs(i, (j + 2)) {
					ans = true
				} else 			if (first && dfs((i + 1), j)) {
					ans = true
				}
			} else {
				if (first && dfs((i + 1), (j + 1))) {
					ans = true
				}
			}
		} else {
			if (first && dfs((i + 1), (j + 1))) {
				ans = true
			}
		}
		memo[key] = ans
		return ans
}
	return dfs(0, 0)
}

func example_1() {
	expect((isMatch("aa", "a") == false))
}

func example_2() {
	expect((isMatch("aa", "a*") == true))
}

func example_3() {
	expect((isMatch("ab", ".*") == true))
}

func example_4() {
	expect((isMatch("aab", "c*a*b") == true))
}

func example_5() {
	expect((isMatch("mississippi", "mis*is*p*.") == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	example_4()
	example_5()
}

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

