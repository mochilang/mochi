package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numDecodings(s string) int {
	var n int = len(s)
	if (n == 0) {
		return 0
	}
	var digits map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var dp []int = []int{}
	var i int = 0
	for (i <= n) {
		dp = append(append([]int{}, dp...), []int{0}...)
		i = (i + 1)
	}
	dp[0] = 1
	if (_indexString(s, 0) != "0") {
		dp[1] = 1
	}
	var idx int = 2
	for (idx <= n) {
		var one string = _indexString(s, (idx - 1))
		if (one != "0") {
			dp[idx] = (dp[idx] + dp[(idx - 1)])
		}
		var d1 int = digits[_indexString(s, (idx - 2))]
		var d2 int = digits[_indexString(s, (idx - 1))]
		var val int = ((d1 * 10) + d2)
		if ((val >= 10) && (val <= 26)) {
			dp[idx] = (dp[idx] + dp[(idx - 2)])
		}
		idx = (idx + 1)
	}
	return dp[n]
}

func example_1() {
	expect((numDecodings("12") == 2))
}

func example_2() {
	expect((numDecodings("226") == 3))
}

func example_3() {
	expect((numDecodings("06") == 0))
}

func single_zero() {
	expect((numDecodings("0") == 0))
}

func _101() {
	expect((numDecodings("2101") == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_zero()
	_101()
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

