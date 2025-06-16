package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxProfit(prices []int) int {
	var n int = len(prices)
	if (n == 0) {
		return 0
	}
	var buy1 int = (0 - prices[0])
	var sell1 int = 0
	var buy2 int = (0 - prices[0])
	var sell2 int = 0
	var i int = 1
	for (i < n) {
		var price int = prices[i]
		var b1 int = (0 - price)
		if (b1 > buy1) {
			buy1 = b1
		}
		var s1 int = (buy1 + price)
		if (s1 > sell1) {
			sell1 = s1
		}
		var b2 int = (sell1 - price)
		if (b2 > buy2) {
			buy2 = b2
		}
		var s2 int = (buy2 + price)
		if (s2 > sell2) {
			sell2 = s2
		}
		i = (i + 1)
	}
	return sell2
}

func example_1() {
	expect((maxProfit([]int{3, 3, 5, 0, 0, 3, 1, 4}) == 6))
}

func example_2() {
	expect((maxProfit([]int{1, 2, 3, 4, 5}) == 4))
}

func example_3() {
	expect((maxProfit([]int{7, 6, 4, 3, 1}) == 0))
}

func single_price() {
	expect((maxProfit([]int{5}) == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_price()
}

