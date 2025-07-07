//go:build ignore

package main

import "fmt"

func main() {
	data := map[string]map[string]int{
		"outer": {"inner": 1},
	}
	data["outer"]["inner"] = 2
	fmt.Println(data["outer"]["inner"])
}
