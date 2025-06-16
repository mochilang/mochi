package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func lengthOfLongestSubstringKDistinct(s string, k int) int {
	if (k <= 0) {
		return 0
	}
	var left int = 0
	var best int = 0
	var counts map[string]int = map[string]int{}
	var distinct int = 0
	var right int = 0
	for (right < len(s)) {
		var ch string = _indexString(s, right)
		_tmp0 := ch
		_tmp1 := counts
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			if (counts[ch] == 0) {
				distinct = (distinct + 1)
			}
			counts[ch] = (counts[ch] + 1)
		} else {
			counts[ch] = 1
			distinct = (distinct + 1)
		}
		for (distinct > k) {
			var leftCh string = _indexString(s, left)
			counts[leftCh] = (counts[leftCh] - 1)
			if (counts[leftCh] == 0) {
				distinct = (distinct - 1)
			}
			left = (left + 1)
		}
		var length int = ((right - left) + 1)
		if (length > best) {
			best = length
		}
		right = (right + 1)
	}
	return best
}

func example_1() {
	expect((lengthOfLongestSubstringKDistinct("eceba", 2) == 3))
}

func example_2() {
	expect((lengthOfLongestSubstringKDistinct("aa", 1) == 2))
}

func empty() {
	expect((lengthOfLongestSubstringKDistinct("", 3) == 0))
}

func k_zero() {
	expect((lengthOfLongestSubstringKDistinct("abc", 0) == 0))
}

func k_larger_than_distinct() {
	expect((lengthOfLongestSubstringKDistinct("abc", 5) == 3))
}

func main() {
	example_1()
	example_2()
	empty()
	k_zero()
	k_larger_than_distinct()
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

