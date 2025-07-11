//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var msg string = "Hello World! "
	var shift int = 0
	var inc int = 1
	var clicks int = 0
	var frames int = 0
	for {
		if !(clicks < 5) {
			break
		}
		var line string = ""
		var i int = 0
		for {
			if !(i < 13) {
				break
			}
			var idx int = ((shift + i) % 13)
			line = line + _sliceString(msg, idx, (idx+1))
			i = (i + 1)
		}
		fmt.Println(line)
		shift = ((shift + inc) % 13)
		frames = (frames + 1)
		if (frames % 13) == 0 {
			inc = (13 - inc)
			clicks = (clicks + 1)
		}
	}
}

func _sliceString(s string, i, j int) string {
	start := i
	end := j
	n := len([]rune(s))
	if start < 0 {
		start += n
	}
	if end < 0 {
		end += n
	}
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if end < start {
		end = start
	}
	return string([]rune(s)[start:end])
}
