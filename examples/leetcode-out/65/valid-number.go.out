package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isNumber(s string) bool {
	var i int = 0
	var n int = len(s)
	for (i < n) {
		if (_indexString(s, i) == " ") {
			i = (i + 1)
		} else {
			break
		}
	}
	if ((i < n) && (((_indexString(s, i) == "+") || (_indexString(s, i) == "-")))) {
		i = (i + 1)
	}
	var num bool = false
	var dot bool = false
	var exp bool = false
	var numAfterExp bool = true
	for (i < n) {
		var c string = _indexString(s, i)
		if (c == " ") {
			break
		}
		if ((c == "+") || (c == "-")) {
			return false
		} else 		if (c == ".") {
			if (dot || exp) {
				return false
			}
			dot = true
		} else 		if ((c == "e") || (c == "E")) {
			if (exp || (!num)) {
				return false
			}
			exp = true
			numAfterExp = false
			if ((i + 1) < n) {
				if ((_indexString(s, (i + 1)) == "+") || (_indexString(s, (i + 1)) == "-")) {
					i = (i + 1)
				}
			}
		} else {
			var digits map[string]bool = map[string]bool{"0": true, "1": true, "2": true, "3": true, "4": true, "5": true, "6": true, "7": true, "8": true, "9": true}
			_tmp0 := c
			_tmp1 := digits
			_, _tmp2 := _tmp1[_tmp0]
			if !(_tmp2) {
				return false
			}
			num = true
			if exp {
				numAfterExp = true
			}
		}
		i = (i + 1)
	}
	for (i < n) {
		if (_indexString(s, i) == " ") {
			i = (i + 1)
		} else {
			break
		}
	}
	return ((num && ((!exp || numAfterExp))) && (i == n))
}

func valid_simple() {
	expect((isNumber("0") == true))
}

func invalid_single_e() {
	expect((isNumber("e") == false))
}

func valid_exponent() {
	expect((isNumber("2e10") == true))
}

func invalid_mixed() {
	expect((isNumber("99e2.5") == false))
}

func spaces_around() {
	expect((isNumber(" 0.1 ") == true))
}

func invalid_sign() {
	expect((isNumber("--6") == false))
}

func main() {
	valid_simple()
	invalid_single_e()
	valid_exponent()
	invalid_mixed()
	spaces_around()
	invalid_sign()
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

