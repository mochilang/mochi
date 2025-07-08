//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var scores map[string]int = map[string]int{"alice": 1}
	scores["bob"] = 2
	fmt.Println(scores["bob"])
}
