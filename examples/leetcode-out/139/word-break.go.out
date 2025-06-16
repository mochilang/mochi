package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func wordBreak(s string, wordDict []string) bool {
	var dict map[string]bool = map[string]bool{}
	for _, w := range wordDict {
		dict[w] = true
	}
	var n int = len(s)
	var dp []bool = []bool{}
	var i int = 0
	for (i <= n) {
		dp = append(append([]bool{}, dp...), []bool{false}...)
		i = (i + 1)
	}
	dp[0] = true
	var idx int = 1
	for (idx <= n) {
		var j int = 0
		for (j < idx) {
			if dp[j] {
				var part string = string([]rune(s)[j:idx])
				_tmp0 := part
				_tmp1 := dict
				_, _tmp2 := _tmp1[_tmp0]
				if _tmp2 {
					dp[idx] = true
					break
				}
			}
			j = (j + 1)
		}
		idx = (idx + 1)
	}
	return dp[n]
}

func example_1() {
	expect((wordBreak("leetcode", []string{"leet", "code"}) == true))
}

func example_2() {
	expect((wordBreak("applepenapple", []string{"apple", "pen"}) == true))
}

func example_3() {
	expect((wordBreak("catsandog", []string{"cats", "dog", "sand", "and", "cat"}) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
}

