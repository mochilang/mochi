package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func minCostII(costs [][]int) int {
	var n int = len(costs)
	if (n == 0) {
		return 0
	}
	var k int = len(costs[0])
	var prevMin int = 0
	var prevSecond int = 0
	var prevColor int = -1
	var i int = 0
	for (i < n) {
		var currMin int = 2147483647
		var currSecond int = 2147483647
		var currColor int = -1
		var j int = 0
		for (j < k) {
			var cost int = costs[i][j]
			if (j == prevColor) {
				cost = (cost + prevSecond)
			} else {
				cost = (cost + prevMin)
			}
			if (cost < currMin) {
				currSecond = currMin
				currMin = cost
				currColor = j
			} else 			if (cost < currSecond) {
				currSecond = cost
			}
			j = (j + 1)
		}
		prevMin = currMin
		prevSecond = currSecond
		prevColor = currColor
		i = (i + 1)
	}
	return prevMin
}

func example_1() {
	expect((minCostII([][]int{[]int{1, 5, 3}, []int{2, 9, 4}}) == 5))
}

func example_2() {
	expect((minCostII([][]int{[]int{1, 3}, []int{2, 4}}) == 5))
}

func single_house() {
	expect((minCostII([][]int{[]int{8, 6, 5}}) == 5))
}

func main() {
	example_1()
	example_2()
	single_house()
}

