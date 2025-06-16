package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func intToRoman(num int) string {
	var values []int = []int{1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1}
	var symbols []string = []string{"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"}
	var result string = ""
	var i int = 0
	for (num > 0) {
		for (num >= values[i]) {
			result = result + symbols[i]
			num = (num - values[i])
		}
		i = (i + 1)
	}
	return result
}

func example_1() {
	expect((intToRoman(3) == "III"))
}

func example_2() {
	expect((intToRoman(58) == "LVIII"))
}

func example_3() {
	expect((intToRoman(1994) == "MCMXCIV"))
}

func small_numbers() {
	expect((intToRoman(4) == "IV"))
	expect((intToRoman(9) == "IX"))
}

func main() {
	example_1()
	example_2()
	example_3()
	small_numbers()
}

