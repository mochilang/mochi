//go:build ignore

package main

import (
	"fmt"
)

func main() {
	data := map[interface{}]interface{}{"outer": map[interface{}]interface{}{"inner": 1}}
	data["outer"]["inner"] = 2
	fmt.Println(data["outer"]["inner"])
}
