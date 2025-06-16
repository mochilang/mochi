package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func wordPatternMatch(pattern string, s string) bool {
	var m int = len(pattern)
	var n int = len(s)
	var dfs func(int, int, map[string]string, map[string]bool) bool
	dfs = func(pi int, si int, mapping map[string]string, used map[string]bool) bool {
		if ((pi == m) && (si == n)) {
			return true
		}
		if ((pi == m) || (si == n)) {
			return false
		}
		var ch string = string([]rune(pattern)[pi:(pi + 1)])
		_tmp0 := ch
		_tmp1 := mapping
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			var word string = mapping[ch]
			var l int = len(word)
			if ((si + l) > n) {
				return false
			}
			if (string([]rune(s)[si:(si + l)]) != word) {
				return false
			}
			return dfs((pi + 1), (si + l), mapping, used)
		}
		var end int = (si + 1)
		for (end <= n) {
			var word string = string([]rune(s)[si:end])
			var already bool = false
			_tmp3 := word
			_tmp4 := used
			_, _tmp5 := _tmp4[_tmp3]
			if _tmp5 {
				already = used[word]
			}
			if !already {
				var nextMap map[string]string = mapping
				var nextUsed map[string]bool = used
				nextMap[ch] = word
				nextUsed[word] = true
				if dfs((pi + 1), end, nextMap, nextUsed) {
					return true
				}
			}
			end = (end + 1)
		}
		return false
}
	return dfs(0, 0, _cast[map[string]string](map[any]any{}), _cast[map[string]bool](map[any]any{}))
}

func example_1() {
	expect((wordPatternMatch("abab", "redblueredblue") == true))
}

func example_2() {
	expect((wordPatternMatch("aaaa", "asdasdasdasd") == true))
}

func example_3() {
	expect((wordPatternMatch("aabb", "xyzabcxzyabc") == false))
}

func main() {
	example_1()
	example_2()
	example_3()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

