package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxProfit(k int, prices []int) int {
	var n int = len(prices)
	if ((n == 0) || (k == 0)) {
		return 0
	}
	if (k >= (n / 2)) {
		var profit int = 0
		for i := 1; i < n; i++ {
			var diff int = (prices[i] - prices[(i - 1)])
			if (diff > 0) {
				profit = (profit + diff)
			}
		}
		return profit
	}
	var buy []int = []int{}
	var sell []int = []int{}
	var idx int = 0
	for (idx < k) {
		buy = append(append([]int{}, buy...), []int{(0 - prices[0])}...)
		sell = append(append([]int{}, sell...), []int{0}...)
		idx = (idx + 1)
	}
	var i int = 1
	for (i < n) {
		var price int = prices[i]
		var b0 int = (0 - price)
		if (b0 > buy[0]) {
			buy[0] = b0
		}
		var s0 int = (buy[0] + price)
		if (s0 > sell[0]) {
			sell[0] = s0
		}
		var j int = 1
		for (j < k) {
			var b int = (sell[(j - 1)] - price)
			if (b > buy[j]) {
				buy[j] = b
			}
			var s int = (buy[j] + price)
			if (s > sell[j]) {
				sell[j] = s
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return sell[(k - 1)]
}

func example_1() {
	expect((maxProfit(2, []int{2, 4, 1}) == 2))
}

func example_2() {
	expect((maxProfit(2, []int{3, 2, 6, 5, 0, 3}) == 7))
}

func empty_prices() {
	expect((maxProfit(3, []int{}) == 0))
}

func unlimited_transactions() {
	expect((maxProfit(100, []int{1, 2, 3, 4, 5}) == 4))
}

func zero_k() {
	expect((maxProfit(0, []int{1, 3, 2, 8}) == 0))
}

func main() {
	example_1()
	example_2()
	empty_prices()
	unlimited_transactions()
	zero_k()
}

