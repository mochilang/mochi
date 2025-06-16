package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func helper(n int, total int) []string {
	if (n == 0) {
		return []string{""}
	}
	if (n == 1) {
		return []string{"0", "1", "8"}
	}
	var prev []string = helper((n - 2), total)
	var result []string = []string{}
	for _, x := range prev {
		if (n != total) {
			result = append(append([]string{}, result...), []string{"0" + x + "0"}...)
		}
		result = append(append([]string{}, result...), []string{"1" + x + "1"}...)
		result = append(append([]string{}, result...), []string{"6" + x + "9"}...)
		result = append(append([]string{}, result...), []string{"8" + x + "8"}...)
		result = append(append([]string{}, result...), []string{"9" + x + "6"}...)
	}
	return result
}

func strobogrammaticInRange(low string, high string) int {
	var m int = len(low)
	var n int = len(high)
	var count int = 0
	var len int = m
	for (len <= n) {
		var nums []string = helper(len, len)
		for _, num := range nums {
			if ((len == m) && (num < low)) {
				continue
			}
			if ((len == n) && (num > high)) {
				continue
			}
			if ((len > 1) && (_indexString(num, 0) == "0")) {
				continue
			}
			count = (count + 1)
		}
		len = (len + 1)
	}
	return count
}

func example_1() {
	expect((strobogrammaticInRange("50", "100") == 3))
}

func example_2() {
	expect((strobogrammaticInRange("0", "0") == 1))
}

func example_3() {
	expect((strobogrammaticInRange("2", "3") == 0))
}

func main() {
	example_1()
	example_2()
	example_3()
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

