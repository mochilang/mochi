package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	var m map[string]int = map[string]int{"a": 1, "b": 2}
	func() { b, _ := json.Marshal(m); fmt.Println(string(b)) }()
}
