package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func climbStairs(n int) int {
	if (n <= 2) {
		return n
	}
	var first int = 1
	var second int = 2
	var i int = 3
	for (i <= n) {
		var next int = (first + second)
		first = second
		second = next
		i = (i + 1)
	}
	return second
}

func example_1() {
	expect((climbStairs(2) == 2))
}

func example_2() {
	expect((climbStairs(3) == 3))
}

func n___4() {
	expect((climbStairs(4) == 5))
}

func n___5() {
	expect((climbStairs(5) == 8))
}

func main() {
	example_1()
	example_2()
	n___4()
	n___5()
}

