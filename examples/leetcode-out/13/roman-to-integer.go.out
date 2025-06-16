package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func romanToInt(s string) int {
	var values map[string]int = map[string]int{"I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000}
	var total int = 0
	var i int = 0
	var n int = len(s)
	for (i < n) {
		var curr int = values[_indexString(s, i)]
		if ((i + 1) < n) {
			var next int = values[_indexString(s, (i + 1))]
			if (curr < next) {
				total = ((total + next) - curr)
				i = (i + 2)
				continue
			}
		}
		total = (total + curr)
		i = (i + 1)
	}
	return total
}

func example_1() {
	expect((romanToInt("III") == 3))
}

func example_2() {
	expect((romanToInt("LVIII") == 58))
}

func example_3() {
	expect((romanToInt("MCMXCIV") == 1994))
}

func subtractive() {
	expect((romanToInt("IV") == 4))
	expect((romanToInt("IX") == 9))
}

func tens() {
	expect((romanToInt("XL") == 40))
	expect((romanToInt("XC") == 90))
}

func main() {
	example_1()
	example_2()
	example_3()
	subtractive()
	tens()
}

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

