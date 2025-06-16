package main

import (
	"strings"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func splitLines(s string) []string {
	var lines []string = []string{}
	var current string = ""
	var i int = 0
	for (i < len(s)) {
		var c string = _indexString(s, i)
		if (c == "\n") {
			lines = append(append([]string{}, lines...), []string{current}...)
			current = ""
		} else {
			current = current + c
		}
		i = (i + 1)
	}
	lines = append(append([]string{}, lines...), []string{current}...)
	return lines
}

func lengthLongestPath(input string) int {
	if (input == "") {
		return 0
	}
	var lines []string = splitLines(input)
	var maxLen int = 0
	var levels map[int]int = map[int]int{}
	var i int = 0
	for (i < len(lines)) {
		var line string = lines[i]
		var depth int = 0
		for ((depth < len(line)) && (_indexString(line, depth) == "\t")) {
			depth = (depth + 1)
		}
		var name string = string([]rune(line)[depth:len(line)])
		var curr int = len(name)
		if (depth > 0) {
			curr = ((levels[(depth - 1)] + 1) + len(name))
		}
		levels[depth] = curr
		if strings.Contains(name, ".") {
			if (curr > maxLen) {
				maxLen = curr
			}
		}
		i = (i + 1)
	}
	return maxLen
}

func example_1() {
	var input string = "dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext"
	_ = input
	expect((lengthLongestPath(input) == 20))
}

func example_2() {
	var input string = "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext"
	_ = input
	expect((lengthLongestPath(input) == 32))
}

func no_files() {
	expect((lengthLongestPath("dir\n\tsubdir") == 0))
}

func main() {
	example_1()
	example_2()
	no_files()
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

