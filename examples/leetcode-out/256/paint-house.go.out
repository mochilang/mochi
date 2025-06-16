package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func min(a int, b int) int {
	if (a < b) {
		return a
	}
	return b
}

func minCost(costs [][]int) int {
	var n int = len(costs)
	if (n == 0) {
		return 0
	}
	var dpR int = costs[0][0]
	var dpG int = costs[0][1]
	var dpB int = costs[0][2]
	var i int = 1
	for (i < n) {
		var r int = (costs[i][0] + min(dpG, dpB))
		var g int = (costs[i][1] + min(dpR, dpB))
		var b int = (costs[i][2] + min(dpR, dpG))
		dpR = r
		dpG = g
		dpB = b
		i = (i + 1)
	}
	return min(dpR, min(dpG, dpB))
}

func example_1() {
	expect((minCost([][]int{[]int{17, 2, 17}, []int{16, 16, 5}, []int{14, 3, 19}}) == 10))
}

func example_2() {
	expect((minCost([][]int{[]int{7, 6, 2}}) == 2))
}

func main() {
	example_1()
	example_2()
}

