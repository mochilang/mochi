package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func readFrom(src string, n int) string {
	var pos int = 0
	var read4 = func(buf []string) int {
		var count int = 0
		var i int = 0
		for ((i < 4) && (pos < len(src))) {
			buf[i] = _indexString(src, pos)
			i = (i + 1)
			pos = (pos + 1)
			count = (count + 1)
		}
		return count
}
	var buf4 []string = []string{"", "", "", ""}
	var result string = ""
	var total int = 0
	for (total < n) {
		var count int = read4(buf4)
		var i int = 0
		for ((i < count) && (total < n)) {
			result = result + buf4[i]
			total = (total + 1)
			i = (i + 1)
		}
		if (count < 4) {
			break
		}
	}
	return result
}

func read_less_than_file() {
	expect((readFrom("leetcode", 5) == "leetc"))
}

func read_exact() {
	expect((readFrom("mochi", 5) == "mochi"))
}

func read_beyond_end() {
	expect((readFrom("hi", 5) == "hi"))
}

func main() {
	read_less_than_file()
	read_exact()
	read_beyond_end()
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

