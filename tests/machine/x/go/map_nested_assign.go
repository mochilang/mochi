//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var _data map[string]map[string]int = map[string]map[string]int{"outer": map[string]int{"inner": 1}}
	_data["outer"]["inner"] = 2
	fmt.Println(_data["outer"]["inner"])
}
