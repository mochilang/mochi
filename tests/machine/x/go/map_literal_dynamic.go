//go:build ignore

package main

import (
	"fmt"
	"strings"
)

func main() {
	var x int = 3
	var y int = 4
	var m map[string]int = map[string]int{"a": x, "b": y}
	fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(m["a"]), fmt.Sprint(m["b"])}, " "), " "))
}
