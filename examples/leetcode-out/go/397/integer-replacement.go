package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func integerReplacement(n int) int {
	var steps int = 0
	var x int = n
	for (x != 1) {
		if ((x % 2) == 0) {
			x = (x / 2)
		} else {
			if (x == 3) {
				x = 2
			} else {
				var mod4 int = (x % 4)
				if (mod4 == 1) {
					x = (x - 1)
				} else {
					x = (x + 1)
				}
			}
		}
		steps = (steps + 1)
	}
	return steps
}

func example_1() {
	expect((integerReplacement(8) == 3))
}

func example_2() {
	expect((integerReplacement(7) == 4))
}

func example_3() {
	expect((integerReplacement(4) == 2))
}

func one() {
	expect((integerReplacement(1) == 0))
}

func three() {
	expect((integerReplacement(3) == 2))
}

func main() {
	example_1()
	example_2()
	example_3()
	one()
	three()
}

