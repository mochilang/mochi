package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxProfit(prices []int) int {
	var n int = len(prices)
	if (n == 0) {
		return 0
	}
	var hold int = -prices[0]
	var sold int = 0
	var rest int = 0
	var i int = 1
	for (i < n) {
		var prevHold int = hold
		var prevSold int = sold
		var prevRest int = rest
		var buy int = (prevRest - prices[i])
		if (buy > prevHold) {
			hold = buy
		} else {
			hold = prevHold
		}
		sold = (prevHold + prices[i])
		if (prevSold > prevRest) {
			rest = prevSold
		} else {
			rest = prevRest
		}
		i = (i + 1)
	}
	if (sold > rest) {
		return sold
	}
	return rest
}

func example_1() {
	expect((maxProfit([]int{1, 2, 3, 0, 2}) == 3))
}

func cooldown() {
	expect((maxProfit([]int{2, 1, 4}) == 3))
}

func single_day() {
	expect((maxProfit([]int{5}) == 0))
}

func main() {
	example_1()
	cooldown()
	single_day()
}

