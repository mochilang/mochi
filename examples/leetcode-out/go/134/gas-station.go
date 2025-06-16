package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canCompleteCircuit(gas []int, cost []int) int {
	var n int = len(gas)
	var total int = 0
	var tank int = 0
	var start int = 0
	var i int = 0
	for (i < n) {
		total = ((total + gas[i]) - cost[i])
		tank = ((tank + gas[i]) - cost[i])
		if (tank < 0) {
			start = (i + 1)
			tank = 0
		}
		i = (i + 1)
	}
	if (total >= 0) {
		return start
	}
	return -1
}

func example_1() {
	expect((canCompleteCircuit([]int{1, 2, 3, 4, 5}, []int{3, 4, 5, 1, 2}) == 3))
}

func example_2() {
	expect((canCompleteCircuit([]int{2, 3, 4}, []int{3, 4, 3}) == (-1)))
}

func single_station() {
	expect((canCompleteCircuit([]int{5}, []int{4}) == 0))
}

func main() {
	example_1()
	example_2()
	single_station()
}

