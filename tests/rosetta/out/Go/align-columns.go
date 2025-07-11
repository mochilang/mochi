//go:build ignore

package main

import (
	"fmt"
)

// line 4
func split(s string, sep string) []string {
	var parts []string = []string{}
	var cur string = ""
	var i int = 0
	for {
		if !(i < len(s)) {
			break
		}
		if ((len(sep) > 0) && ((i + len(sep)) <= len(s))) && (string([]rune(s)[i:(i+len(sep))]) == sep) {
			parts = append(_convSlice[string, any](parts), cur)
			cur = ""
			i = (i + len(sep))
		} else {
			cur = cur + string([]rune(s)[i:(i+1)])
			i = (i + 1)
		}
	}
	parts = append(_convSlice[string, any](parts), cur)
	return parts
}

// line 22
func rstripEmpty(words []string) []string {
	var n int = len(words)
	for {
		if !((n > 0) && (words[(n-1)] == "")) {
			break
		}
		n = (n - 1)
	}
	return words[0:n]
}

// line 30
func spaces(n int) string {
	var out string = ""
	var i int = 0
	for {
		if !(i < n) {
			break
		}
		out = out + " "
		i = (i + 1)
	}
	return out
}

// line 40
func pad(word string, width int, align int) string {
	var diff int = (width - len(word))
	if align == 0 {
		return word + spaces(diff)
	}
	if align == 2 {
		return spaces(diff) + word
	}
	var left int = int((float64(diff) / float64(2)))
	var right int = (diff - left)
	return spaces(left) + word + spaces(right)
}

// line 53
func newFormatter(text string) map[string]any {
	var lines []string = split(text, "\n")
	var fmtLines [][]string = [][]string{}
	var width []int = []int{}
	var i int = 0
	for {
		if !(i < len(lines)) {
			break
		}
		if len(lines[i]) == 0 {
			i = (i + 1)
			continue
		}
		var words []string = rstripEmpty(split(lines[i], "$"))
		fmtLines = append(_convSlice[[]string, any](fmtLines), words)
		var j int = 0
		for {
			if !(j < len(words)) {
				break
			}
			var wlen int = len(words[j])
			if j == len(width) {
				width = append(_convSlice[int, any](width), wlen)
			} else if wlen > width[j] {
				width[j] = wlen
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return map[string]any{"text": fmtLines, "width": width}
}

// line 80
func printFmt(f map[string]any, align int) {
	var lines [][]string = (f["text"]).([][]string)
	var width []int = (f["width"]).([]int)
	var i int = 0
	for {
		if !(i < len(lines)) {
			break
		}
		var words []string = lines[i]
		var line string = ""
		var j int = 0
		for {
			if !(j < len(words)) {
				break
			}
			line = line + pad(words[j], width[j], align) + " "
			j = (j + 1)
		}
		fmt.Println(line)
		i = (i + 1)
	}
	fmt.Println("")
}

var text string
var f map[string]any

func main() {
	text = "Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n" + "column$are$separated$by$at$least$one$space.\n" + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n" + "justified,$right$justified,$or$center$justified$within$its$column."
	f = newFormatter(text)
	printFmt(f, 0)
	printFmt(f, 1)
	printFmt(f, 2)
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}
