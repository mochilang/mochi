package main

import (
	"fmt"
)

func main() {
	var m map[string]int = map[string]int{"a": 1, "b": 2}
	fmt.Println(m["b"])
}
