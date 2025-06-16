package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getSum(a int, b int) int {
	return (a + b)
}

func example_1() {
	expect((3 == 3))
}

func example_2() {
	expect((5 == 5))
}

func negative() {
	expect((getSum(-2, 3) == 1))
}

func zero() {
	expect((0 == 0))
}

func main() {
	example_1()
	example_2()
	negative()
	zero()
}

