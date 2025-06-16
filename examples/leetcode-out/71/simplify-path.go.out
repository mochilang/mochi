package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func simplifyPath(path string) string {
	var stack []string = []string{}
	var part string = ""
	var i int = 0
	var n int = len(path)
	for (i <= n) {
		if (i == n) {
			if (part == "..") {
				if (len(stack) > 0) {
					stack = stack[0:(len(stack) - 1)]
				}
			} else 			if ((part != "") && (part != ".")) {
				stack = append(append([]string{}, stack...), []string{part}...)
			}
			part = ""
		} else 		if (_indexString(path, i) == "/") {
			if (part == "..") {
				if (len(stack) > 0) {
					stack = stack[0:(len(stack) - 1)]
				}
			} else 			if ((part != "") && (part != ".")) {
				stack = append(append([]string{}, stack...), []string{part}...)
			}
			part = ""
		} else {
			part = part + _indexString(path, i)
		}
		i = (i + 1)
	}
	var result string = "/"
	var j int = 0
	for (j < len(stack)) {
		result = result + stack[j]
		if (j < (len(stack) - 1)) {
			result = result + "/"
		}
		j = (j + 1)
	}
	return result
}

func example_1() {
	expect((simplifyPath("/home/") == "/home"))
}

func example_2() {
	expect((simplifyPath("/../") == "/"))
}

func example_3() {
	expect((simplifyPath("/home//foo/") == "/home/foo"))
}

func complex() {
	expect((simplifyPath("/a/./b/../../c/") == "/c"))
}

func dots() {
	expect((simplifyPath("/a/../../b/../c//.//") == "/c"))
}

func long() {
	expect((simplifyPath("/a//b////c/d//././/..") == "/a/b/c"))
}

func main() {
	example_1()
	example_2()
	example_3()
	complex()
	dots()
	long()
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

