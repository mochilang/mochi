package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func bulbSwitch(n int) int {
	var count int = 0
	var i int = 1
	for ((i * i) <= n) {
		count = (count + 1)
		i = (i + 1)
	}
	return count
}

func example_1() {
	expect((bulbSwitch(3) == 1))
}

func example_2() {
	expect((bulbSwitch(0) == 0))
}

func example_3() {
	expect((bulbSwitch(1) == 1))
}

func larger_n() {
	expect((bulbSwitch(9999) == 99))
}

func main() {
	example_1()
	example_2()
	example_3()
	larger_n()
}

