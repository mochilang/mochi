package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findNthDigit(n int) int {
	var remaining int = n
	var digits int = 1
	var count int = 9
	var start int = 1
	for (remaining > (digits * count)) {
		remaining = (remaining - (digits * count))
		digits = (digits + 1)
		count = (count * 10)
		start = (start * 10)
	}
	var num int = (start + (((remaining - 1)) / digits))
	var index int = (((remaining - 1)) % digits)
	var s string = fmt.Sprint(num)
	var values map[string]int = map[string]int{"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	var ch string = _indexString(s, index)
	return values[ch]
}

func example_1() {
	expect((findNthDigit(3) == 3))
}

func example_2() {
	expect((findNthDigit(11) == 0))
}

func example_3() {
	expect((findNthDigit(12) == 1))
}

func end_of_two_digits() {
	expect((findNthDigit(189) == 9))
}

func start_of_three_digits() {
	expect((findNthDigit(190) == 1))
}

func middle_of_three_digits() {
	expect((findNthDigit(191) == 0))
}

func third_of_three_digits() {
	expect((findNthDigit(192) == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
	end_of_two_digits()
	start_of_three_digits()
	middle_of_three_digits()
	third_of_three_digits()
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

