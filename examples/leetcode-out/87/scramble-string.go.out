package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isScramble(s1 string, s2 string) bool {
	if (len(s1) != len(s2)) {
		return false
	}
	var sameLetters = func(a string, b string) bool {
		if (len(a) != len(b)) {
			return false
		}
		var count map[string]int = map[string]int{}
		var i int = 0
		for (i < len(a)) {
			var ch string = _indexString(a, i)
			_tmp0 := ch
			_tmp1 := count
			_, _tmp2 := _tmp1[_tmp0]
			if _tmp2 {
				count[ch] = (count[ch] + 1)
			} else {
				count[ch] = 1
			}
			i = (i + 1)
		}
		i = 0
		for (i < len(b)) {
			var ch string = _indexString(b, i)
			_tmp3 := ch
			_tmp4 := count
			_, _tmp5 := _tmp4[_tmp3]
			if _tmp5 {
				count[ch] = (count[ch] - 1)
			} else {
				return false
			}
			i = (i + 1)
		}
		for key := range count {
			if (count[key] != 0) {
				return false
			}
		}
		return true
}
	if !sameLetters(s1, s2) {
		return false
	}
	var memo map[int]bool = map[int]bool{}
	var n int = len(s1)
	var dfs func(int, int, int) bool
	dfs = func(i1 int, i2 int, length int) bool {
		var key int = ((((i1 * n) * 31) + (i2 * 31)) + length)
		_tmp0 := key
		_tmp1 := memo
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			return memo[key]
		}
		var a string = string([]rune(s1)[i1:(i1 + length)])
		var b string = string([]rune(s2)[i2:(i2 + length)])
		if (a == b) {
			memo[key] = true
			return true
		}
		if !sameLetters(a, b) {
			memo[key] = false
			return false
		}
		var k int = 1
		for (k < length) {
			if (dfs(i1, i2, k) && dfs((i1 + k), (i2 + k), (length - k))) {
				memo[key] = true
				return true
			}
			if (dfs(i1, ((i2 + length) - k), k) && dfs((i1 + k), i2, (length - k))) {
				memo[key] = true
				return true
			}
			k = (k + 1)
		}
		memo[key] = false
		return false
}
	return dfs(0, 0, n)
}

func example_1() {
	expect((isScramble("great", "rgeat") == true))
}

func example_2() {
	expect((isScramble("abcde", "caebd") == false))
}

func example_3() {
	expect((isScramble("a", "a") == true))
}

func main() {
	example_1()
	example_2()
	example_3()
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

