package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func fractionToDecimal(numerator int, denominator int) string {
	if (denominator == 0) {
		return ""
	}
	if (numerator == 0) {
		return "0"
	}
	var result string = ""
	var minus string = _indexString(fmt.Sprint(-1), 0)
	var negative bool = false
	if ((((numerator < 0) && (denominator > 0))) || (((numerator > 0) && (denominator < 0)))) {
		negative = true
	}
	if (numerator < 0) {
		numerator = -numerator
	}
	if (denominator < 0) {
		denominator = -denominator
	}
	var integerPart int = (numerator / denominator)
	result = fmt.Sprint(integerPart)
	var remainder int = (numerator % denominator)
	if (remainder == 0) {
		if negative {
			return minus + result
		}
		return result
	}
	result = result + "."
	var mapIndex map[int]int = map[int]int{}
	var decimal string = ""
	for (remainder != 0) {
		_tmp0 := remainder
		_tmp1 := mapIndex
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			var idx int = mapIndex[remainder]
			var prefix string = string([]rune(decimal)[0:idx])
			var suffix string = string([]rune(decimal)[idx:len(decimal)])
			decimal = prefix + "(" + suffix + ")"
			break
		}
		mapIndex[remainder] = len(decimal)
		remainder = (remainder * 10)
		var digit int = (remainder / denominator)
		decimal = decimal + fmt.Sprint(digit)
		remainder = (remainder % denominator)
	}
	result = result + decimal
	if negative {
		return minus + result
	}
	return result
}

func example_1() {
	expect((fractionToDecimal(1, 2) == "0.5"))
}

func example_2() {
	expect((fractionToDecimal(2, 1) == "2"))
}

func example_3() {
	expect((fractionToDecimal(2, 3) == "0.(6)"))
}

func negative() {
	expect((fractionToDecimal(-50, 8) == "-6.25"))
}

func repeat_zeros() {
	expect((fractionToDecimal(1, 6) == "0.1(6)"))
}

func main() {
	example_1()
	example_2()
	example_3()
	negative()
	repeat_zeros()
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

