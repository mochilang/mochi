//go:build ignore

package main

import (
	"fmt"
)

func main() {
	prefix := "fore"
	s1 := "forest"
	fmt.Println(s1[0:len(prefix)] == prefix)
	s2 := "desert"
	fmt.Println(s2[0:len(prefix)] == prefix)
}
