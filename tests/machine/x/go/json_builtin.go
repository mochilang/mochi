//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	m := M{
		1,
		2,
	}
	func() { b, _ := json.Marshal(m); fmt.Println(string(b)) }()
}
