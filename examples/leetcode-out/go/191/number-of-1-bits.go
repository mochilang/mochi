package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func hammingWeight(n int) int {
	var count int = 0
	var num int = n
	for (num > 0) {
		if ((num % 2) == 1) {
			count = (count + 1)
		}
		num = (num / 2)
	}
	return count
}

func example_1() {
	expect((hammingWeight(11) == 3))
}

func example_2() {
	expect((hammingWeight(128) == 1))
}

func example_3() {
	expect((hammingWeight(4294967293) == 31))
}

func zero() {
	expect((hammingWeight(0) == 0))
}

func all_ones() {
	expect((hammingWeight(4294967295) == 32))
}

func main() {
	example_1()
	example_2()
	example_3()
	zero()
	all_ones()
}

