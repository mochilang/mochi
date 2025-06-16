package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func reverseBits(n int) int {
	var result int = 0
	var x int = n
	var count int = 0
	for (count < 32) {
		var bit int = (x % 2)
		result = ((result * 2) + bit)
		x = (x / 2)
		count = (count + 1)
	}
	return result
}

func example_1() {
	expect((reverseBits(43261596) == 964176192))
}

func example_2() {
	expect((reverseBits(4294967293) == 3221225471))
}

func main() {
	example_1()
	example_2()
}

