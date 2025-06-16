package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isSelfCrossing(distance []int) bool {
	var n int = len(distance)
	if (n < 4) {
		return false
	}
	var i int = 3
	for (i < n) {
		if ((distance[i] >= distance[(i - 2)]) && (distance[(i - 1)] <= distance[(i - 3)])) {
			return true
		}
		if (((i >= 4) && (distance[(i - 1)] == distance[(i - 3)])) && ((distance[i] + distance[(i - 4)]) >= distance[(i - 2)])) {
			return true
		}
		if (((((i >= 5) && (distance[(i - 2)] >= distance[(i - 4)])) && (distance[(i - 1)] <= distance[(i - 3)])) && ((distance[i] + distance[(i - 4)]) >= distance[(i - 2)])) && ((distance[(i - 1)] + distance[(i - 5)]) >= distance[(i - 3)])) {
			return true
		}
		i = (i + 1)
	}
	return false
}

func example_1() {
	expect((isSelfCrossing([]int{2, 1, 1, 2}) == true))
}

func example_2() {
	expect((isSelfCrossing([]int{1, 2, 3, 4}) == false))
}

func example_3() {
	expect((isSelfCrossing([]int{1, 1, 1, 2, 1}) == true))
}

func short() {
	expect((isSelfCrossing([]int{1, 2, 1}) == false))
}

func no_crossing() {
	expect((isSelfCrossing([]int{3, 3, 4, 2, 2}) == false))
}

func cross_late() {
	expect((isSelfCrossing([]int{1, 1, 2, 1, 1}) == true))
}

func main() {
	example_1()
	example_2()
	example_3()
	short()
	no_crossing()
	cross_late()
}

