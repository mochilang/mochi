//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	m := map[string]int{"a": 1, "b": 2}
	b, _ := json.Marshal(m)
	fmt.Println(string(b))
}
