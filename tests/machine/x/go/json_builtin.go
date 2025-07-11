//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	type M struct {
		A int `json:"a"`
		B int `json:"b"`
	}

	var m M = M{
		1,
		2,
	}
	func() { b, _ := json.Marshal(m); fmt.Println(string(b)) }()
}
