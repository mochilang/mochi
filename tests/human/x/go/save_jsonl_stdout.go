//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	people := []Person{{"Alice", 30}, {"Bob", 25}}
	for _, p := range people {
		b, _ := json.Marshal(p)
		fmt.Println(string(b))
	}
}
