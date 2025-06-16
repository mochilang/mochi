package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canWinNim(n int) bool {
	return ((n % 4) != 0)
}

func example_1() {
	expect((canWinNim(4) == false))
}

func example_2() {
	expect((canWinNim(1) == true))
}

func example_3() {
	expect((canWinNim(2) == true))
}

func multiple_of_four() {
	expect((canWinNim(8) == false))
}

func not_multiple_of_four() {
	expect((canWinNim(7) == true))
}

func main() {
	example_1()
	example_2()
	example_3()
	multiple_of_four()
	not_multiple_of_four()
}

