package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func longestSubstring(s string, k int) int {
	if (len(s) == 0) {
		return 0
	}
	var maxLen int = 0
	var target int = 1
	for (target <= 26) {
		var counts map[string]int = map[string]int{}
		var left int = 0
		var right int = 0
		var unique int = 0
		var atLeast int = 0
		for (right < len(s)) {
			var ch string = _indexString(s, right)
			_tmp0 := ch
			_tmp1 := counts
			_, _tmp2 := _tmp1[_tmp0]
			if _tmp2 {
				counts[ch] = (counts[ch] + 1)
			} else {
				counts[ch] = 1
				unique = (unique + 1)
			}
			if (counts[ch] == k) {
				atLeast = (atLeast + 1)
			}
			for (unique > target) {
				var leftCh string = _indexString(s, left)
				if (counts[leftCh] == k) {
					atLeast = (atLeast - 1)
				}
				counts[leftCh] = (counts[leftCh] - 1)
				if (counts[leftCh] == 0) {
					unique = (unique - 1)
				}
				left = (left + 1)
			}
			if ((unique == target) && (atLeast == target)) {
				var length int = ((right - left) + 1)
				if (length > maxLen) {
					maxLen = length
				}
			}
			right = (right + 1)
		}
		target = (target + 1)
	}
	return maxLen
}

func example_1() {
	expect((longestSubstring("aaabb", 3) == 3))
}

func example_2() {
	expect((longestSubstring("ababbc", 2) == 5))
}

func all_same() {
	expect((longestSubstring("aaaaa", 1) == 5))
}

func no_valid_substring() {
	expect((longestSubstring("abcde", 2) == 0))
}

func empty_string() {
	expect((longestSubstring("", 3) == 0))
}

func main() {
	example_1()
	example_2()
	all_same()
	no_valid_substring()
	empty_string()
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

