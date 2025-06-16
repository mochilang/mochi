package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func splitComma(s string) []string {
	var parts []string = []string{}
	var current string = ""
	var i int = 0
	for (i < len(s)) {
		var c string = _indexString(s, i)
		if (c == ",") {
			parts = append(append([]string{}, parts...), []string{current}...)
			current = ""
		} else {
			current = current + c
		}
		i = (i + 1)
	}
	parts = append(append([]string{}, parts...), []string{current}...)
	return parts
}

func isValidSerialization(preorder string) bool {
	var nodes []string = splitComma(preorder)
	var slots int = 1
	var i int = 0
	for (i < len(nodes)) {
		slots = (slots - 1)
		if (slots < 0) {
			return false
		}
		if (nodes[i] != "#") {
			slots = (slots + 2)
		}
		i = (i + 1)
	}
	return (slots == 0)
}

func example_1() {
	expect((isValidSerialization("9,3,4,#,#,1,#,#,2,#,6,#,#") == true))
}

func example_2() {
	expect((isValidSerialization("1,#") == false))
}

func example_3() {
	expect((isValidSerialization("9,#,#,1") == false))
}

func empty_tree() {
	expect((isValidSerialization("#") == true))
}

func main() {
	example_1()
	example_2()
	example_3()
	empty_tree()
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

