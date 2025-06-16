package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxCoins(nums []int) int {
	var arr []int = []int{1}
	arr = append(append([]int{}, arr...), nums...)
	arr = append(append([]int{}, arr...), []int{1}...)
	var n int = len(arr)
	var dp [][]int = [][]int{}
	var i int = 0
	for (i < n) {
		var row []int = []int{}
		var j int = 0
		for (j < n) {
			row = append(append([]int{}, row...), []int{0}...)
			j = (j + 1)
		}
		dp = append(append([][]int{}, dp...), [][]int{row}...)
		i = (i + 1)
	}
	var length int = 2
	for (length < n) {
		var left int = 0
		for ((left + length) < n) {
			var right int = (left + length)
			var k int = (left + 1)
			for (k < right) {
				var coins int = ((((arr[left] * arr[k]) * arr[right]) + dp[left][k]) + dp[k][right])
				if (coins > dp[left][right]) {
					dp[left][right] = coins
				}
				k = (k + 1)
			}
			left = (left + 1)
		}
		length = (length + 1)
	}
	return dp[0][(n - 1)]
}

func example_1() {
	expect((maxCoins([]int{3, 1, 5, 8}) == 167))
}

func example_2() {
	expect((maxCoins([]int{1, 5}) == 10))
}

func main() {
	example_1()
	example_2()
}

