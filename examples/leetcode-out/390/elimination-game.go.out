package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func lastRemaining(n int) int {
	var head int = 1
	var step int = 1
	var remaining int = n
	var left bool = true
	for (remaining > 1) {
		if (left || ((remaining % 2) == 1)) {
			head = (head + step)
		}
		remaining = (remaining / 2)
		step = (step * 2)
		left = !left
	}
	return head
}

func example_1() {
	expect((lastRemaining(9) == 6))
}

func example_2() {
	expect((lastRemaining(1) == 1))
}

func even_n() {
	expect((lastRemaining(2) == 2))
}

func larger_n() {
	expect((lastRemaining(1000000) == 481110))
}

func main() {
	example_1()
	example_2()
	even_n()
	larger_n()
}

