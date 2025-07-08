//go:build ignore

package main

import (
	"fmt"
)

func main() {
	scores := map[interface{}]interface{}{"alice": 1}
	scores["bob"] = 2
	fmt.Println(scores["bob"])
}
