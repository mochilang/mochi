package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numWays(n int, k int) int {
	if ((n == 0) || (k == 0)) {
		return 0
	}
	if (n == 1) {
		return k
	}
	var same int = 0
	var diff int = k
	var i int = 2
	for (i <= n) {
		var prevDiff int = diff
		diff = (((same + diff)) * ((k - 1)))
		same = prevDiff
		i = (i + 1)
	}
	return (same + diff)
}

func example_1() {
	expect((numWays(3, 2) == 6))
}

func example_2() {
	expect((numWays(1, 1) == 1))
}

func no_posts() {
	expect((numWays(0, 5) == 0))
}

func one_color() {
	expect((numWays(3, 1) == 0))
}

func main() {
	example_1()
	example_2()
	no_posts()
	one_color()
}

