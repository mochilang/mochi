package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countDigitOne(n int) int {
	var count int = 0
	var i int = 1
	for (i <= n) {
		var high int = (n / ((i * 10)))
		var current int = (((n / i)) % 10)
		var low int = (n % i)
		if (current == 0) {
			count = (count + (high * i))
		} else 		if (current == 1) {
			count = ((count + (high * i)) + ((low + 1)))
		} else {
			count = (count + (((high + 1)) * i))
		}
		i = (i * 10)
	}
	return count
}

func example_1() {
	expect((countDigitOne(13) == 6))
}

func example_2() {
	expect((countDigitOne(0) == 0))
}

func single_digit() {
	expect((countDigitOne(1) == 1))
}

func two_digits() {
	expect((countDigitOne(20) == 12))
}

func hundreds() {
	expect((countDigitOne(111) == 36))
}

func main() {
	example_1()
	example_2()
	single_digit()
	two_digits()
	hundreds()
}

