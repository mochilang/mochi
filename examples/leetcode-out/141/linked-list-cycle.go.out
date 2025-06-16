package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func hasCycle(values []int, pos int) bool {
	var n int = len(values)
	var nextIndex = func(i int) int {
		if (i == (n - 1)) {
			if (pos >= 0) {
				return pos
			} else {
				return n
			}
		} else {
			return (i + 1)
		}
}
	if ((n == 0) || (pos < 0)) {
		return false
	}
	var slow int = 0
	var fast int = 0
	for {
		slow = nextIndex(slow)
		fast = nextIndex(fast)
		if (fast >= n) {
			return false
		}
		fast = nextIndex(fast)
		if ((slow >= n) || (fast >= n)) {
			return false
		}
		if (slow == fast) {
			return true
		}
	}
	return false
}

func example_1() {
	expect((hasCycle([]int{3, 2, 0, -4}, 1) == true))
}

func example_2() {
	expect((hasCycle([]int{1, 2}, 0) == true))
}

func example_3() {
	expect((hasCycle([]int{1}, -1) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
}

