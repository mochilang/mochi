package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isInterleave(s1 string, s2 string, s3 string) bool {
	var m int = len(s1)
	var n int = len(s2)
	if ((m + n) != len(s3)) {
		return false
	}
	var dp [][]bool = [][]bool{}
	var i int = 0
	for (i <= m) {
		var row []bool = []bool{}
		var j int = 0
		for (j <= n) {
			row = append(append([]bool{}, row...), []bool{false}...)
			j = (j + 1)
		}
		dp = append(append([][]bool{}, dp...), [][]bool{row}...)
		i = (i + 1)
	}
	dp[0][0] = true
	i = 0
	for (i <= m) {
		var j int = 0
		for (j <= n) {
			if (i > 0) {
				if (dp[(i - 1)][j] && (_indexString(s1, (i - 1)) == _indexString(s3, ((i + j) - 1)))) {
					dp[i][j] = true
				}
			}
			if (j > 0) {
				if (dp[i][(j - 1)] && (_indexString(s2, (j - 1)) == _indexString(s3, ((i + j) - 1)))) {
					dp[i][j] = true
				}
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return dp[m][n]
}

func example_1() {
	expect((isInterleave("aabcc", "dbbca", "aadbbcbcac") == true))
}

func example_2() {
	expect((isInterleave("aabcc", "dbbca", "aadbbbaccc") == false))
}

func empty() {
	expect((isInterleave("", "", "") == true))
}

func main() {
	example_1()
	example_2()
	empty()
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

