package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getHint(secret string, guess string) string {
	var bulls int = 0
	var countSecret map[string]int = map[string]int{}
	var countGuess map[string]int = map[string]int{}
	var i int = 0
	for (i < len(secret)) {
		var s string = _indexString(secret, i)
		var g string = _indexString(guess, i)
		if (s == g) {
			bulls = (bulls + 1)
		} else {
			_tmp0 := s
			_tmp1 := countSecret
			_, _tmp2 := _tmp1[_tmp0]
			if _tmp2 {
				countSecret[s] = (countSecret[s] + 1)
			} else {
				countSecret[s] = 1
			}
			_tmp3 := g
			_tmp4 := countGuess
			_, _tmp5 := _tmp4[_tmp3]
			if _tmp5 {
				countGuess[g] = (countGuess[g] + 1)
			} else {
				countGuess[g] = 1
			}
		}
		i = (i + 1)
	}
	var cows int = 0
	for ch := range countSecret {
		_tmp6 := ch
		_tmp7 := countGuess
		_, _tmp8 := _tmp7[_tmp6]
		if _tmp8 {
			var a int = countSecret[ch]
			var b int = countGuess[ch]
			if (a < b) {
				cows = (cows + a)
			} else {
				cows = (cows + b)
			}
		}
	}
	return fmt.Sprint(bulls) + "A" + fmt.Sprint(cows) + "B"
}

func example_1() {
	expect((getHint("1807", "7810") == "1A3B"))
}

func example_2() {
	expect((getHint("1123", "0111") == "1A1B"))
}

func all_bulls() {
	expect((getHint("1234", "1234") == "4A0B"))
}

func all_cows() {
	expect((getHint("1122", "2211") == "0A4B"))
}

func no_matches() {
	expect((getHint("1234", "5678") == "0A0B"))
}

func main() {
	example_1()
	example_2()
	all_bulls()
	all_cows()
	no_matches()
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

