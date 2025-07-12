//go:build ignore

package main

import (
	"fmt"
)

func main() {
	prefix := "fore"
	s1 := "forest"
	fmt.Println((string([]rune(s1)[0:4]) == prefix))
	s2 := "desert"
	fmt.Println((string([]rune(s2)[0:4]) == prefix))
}
