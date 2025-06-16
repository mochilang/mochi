package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func canWin(s string) bool {
	var memo map[string]bool = map[string]bool{}
	var helper func(string) bool
	helper = func(cur string) bool {
		_tmp0 := cur
		_tmp1 := memo
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			return memo[cur]
		}
		var i int = 0
		for ((i + 1) < len(cur)) {
			if ((_indexString(cur, i) == "+") && (_indexString(cur, (i + 1)) == "+")) {
				var next string = string([]rune(cur)[0:i]) + "--" + string([]rune(cur)[(i + 2):len(cur)])
				if !helper(next) {
					memo[cur] = true
					return true
				}
			}
			i = (i + 1)
		}
		memo[cur] = false
		return false
}
	return helper(s)
}

func example_1() {
	expect((canWin("++++") == true))
}

func example_2() {
	expect((canWin("+") == false))
}

func five_plus() {
	expect((canWin("+++++") == false))
}

func mixed() {
	expect((canWin("+-++") == true))
}

func main() {
	example_1()
	example_2()
	five_plus()
	mixed()
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

