//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var door int = 1
	var incrementer int = 0
	for current := 1; current < 101; current++ {
		var line string = "Door " + fmt.Sprint(current) + " "
		if current == door {
			line = line + "Open"
			incrementer = (incrementer + 1)
			door = ((door + (2 * incrementer)) + 1)
		} else {
			line = line + "Closed"
		}
		fmt.Println(line)
	}
}
