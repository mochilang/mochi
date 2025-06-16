package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countAndSay(n int) string {
	var term string = "1"
	var i int = 1
	for (i < n) {
		var next string = ""
		var j int = 0
		for (j < len(term)) {
			var k int = j
			for (k < len(term)) {
				if (_indexString(term, k) == _indexString(term, j)) {
					k = (k + 1)
				} else {
					break
				}
			}
			var count int = (k - j)
			next = next + fmt.Sprint(count) + _indexString(term, j)
			j = k
		}
		term = next
		i = (i + 1)
	}
	return term
}

func n___1() {
	expect((countAndSay(1) == "1"))
}

func n___4() {
	expect((countAndSay(4) == "1211"))
}

func n___5() {
	expect((countAndSay(5) == "111221"))
}

func main() {
	n___1()
	n___4()
	n___5()
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

