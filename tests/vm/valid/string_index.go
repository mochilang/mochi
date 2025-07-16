package main

import (
	"fmt"
)

func main() {
	var s string = "mochi"
	fmt.Println(_indexString(s, 1))
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
