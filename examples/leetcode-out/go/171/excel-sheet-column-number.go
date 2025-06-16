package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func titleToNumber(columnTitle string) int {
	var values map[string]int = map[string]int{"A": 1, "B": 2, "C": 3, "D": 4, "E": 5, "F": 6, "G": 7, "H": 8, "I": 9, "J": 10, "K": 11, "L": 12, "M": 13, "N": 14, "O": 15, "P": 16, "Q": 17, "R": 18, "S": 19, "T": 20, "U": 21, "V": 22, "W": 23, "X": 24, "Y": 25, "Z": 26}
	var result int = 0
	for _, r := range []rune(columnTitle) {
		ch := string(r)
		result = ((result * 26) + values[ch])
	}
	return result
}

func example_1() {
	expect((titleToNumber("A") == 1))
}

func example_2() {
	expect((titleToNumber("AB") == 28))
}

func example_3() {
	expect((titleToNumber("ZY") == 701))
}

func single_Z() {
	expect((titleToNumber("Z") == 26))
}

func large_input() {
	expect((titleToNumber("FXSHRXW") == 2147483647))
}

func main() {
	example_1()
	example_2()
	example_3()
	single_Z()
	large_input()
}

