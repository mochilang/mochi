//go:build ignore

// Generated by Mochi v0.10.36 on 2025-07-22 18:26:23 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"strings"
)

var a []int = []int{1, 2}

func main() {
	fmt.Println(func() string {
		b, _ := json.Marshal(append(a, 3))
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		s = strings.ReplaceAll(s, "}, {", "},{")
		s = strings.ReplaceAll(s, "\"", "'")
		return s
	}())
}
