//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var prefix string = "fore"
	var s1 string = "forest"
	fmt.Println((string([]rune(s1)[0:4]) == prefix))
	var s2 string = "desert"
	fmt.Println((string([]rune(s2)[0:4]) == prefix))
}
