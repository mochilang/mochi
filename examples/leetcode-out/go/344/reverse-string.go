package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func toList(s string) []string {
	var out []string = []string{}
	for i := 0; i < len(s); i++ {
		out = append(append([]string{}, out...), []string{_indexString(s, i)}...)
	}
	return out
}

func fromList(arr []string) string {
	var out string = ""
	for _, ch := range arr {
		out = out + ch
	}
	return out
}

func reverseString(chars []string) []string {
	var arr []string = chars
	var left int = 0
	var right int = (len(arr) - 1)
	for (left < right) {
		var temp string = arr[left]
		arr[left] = arr[right]
		arr[right] = temp
		left = (left + 1)
		right = (right - 1)
	}
	return arr
}

func example_1() {
	var input []string = toList("hello")
	var result []string = reverseString(input)
	_ = result
	expect((fromList(result) == "olleh"))
}

func example_2() {
	var input []string = toList("Hannah")
	var result []string = reverseString(input)
	_ = result
	expect((fromList(result) == "hannaH"))
}

func empty() {
	var input []string = []string{}
	var result []string = reverseString(input)
	_ = result
	expect((len(result) == 0))
}

func main() {
	example_1()
	example_2()
	empty()
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

