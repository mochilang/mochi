package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maximalSquare(matrix [][]string) int {
	var rows int = len(matrix)
	if (rows == 0) {
		return 0
	}
	var cols int = len(matrix[0])
	var dp [][]int = [][]int{}
	var r int = 0
	for (r < rows) {
		var row []int = []int{}
		var c int = 0
		for (c < cols) {
			row = append(append([]int{}, row...), []int{0}...)
			c = (c + 1)
		}
		dp = append(append([][]int{}, dp...), [][]int{row}...)
		r = (r + 1)
	}
	var maxSide int = 0
	r = 0
	for (r < rows) {
		var c int = 0
		for (c < cols) {
			if (matrix[r][c] == "1") {
				if ((r == 0) || (c == 0)) {
					dp[r][c] = 1
				} else {
					var top int = dp[(r - 1)][c]
					var left int = dp[r][(c - 1)]
					var diag int = dp[(r - 1)][(c - 1)]
					var small int = top
					if (left < small) {
						small = left
					}
					if (diag < small) {
						small = diag
					}
					dp[r][c] = (small + 1)
				}
				if (dp[r][c] > maxSide) {
					maxSide = dp[r][c]
				}
			} else {
				dp[r][c] = 0
			}
			c = (c + 1)
		}
		r = (r + 1)
	}
	return (maxSide * maxSide)
}

func example_1() {
	expect((maximalSquare(example1) == 4))
}

func example_2() {
	expect((maximalSquare(example2) == 1))
}

func single_zero() {
	expect((maximalSquare(example3) == 0))
}

var example1 [][]string = [][]string{[]string{"1", "0", "1", "0", "0"}, []string{"1", "0", "1", "1", "1"}, []string{"1", "1", "1", "1", "1"}, []string{"1", "0", "0", "1", "0"}}
var example2 [][]string = [][]string{[]string{"0", "1"}, []string{"1", "0"}}
var example3 [][]string = [][]string{[]string{"0"}}
func main() {
	example_1()
	example_2()
	single_zero()
}

