//go:build ignore

// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
)

type v map[string]any

func main() {
	var door int = 1
	var incrementer int = 0
	for current := 1; current < 101; current++ {
		var line string = "Door " + fmt.Sprint(any(current)) + " "
		if current == door {
			line = line + "Open"
			incrementer = (incrementer + 1)
			door = ((door + (2 * incrementer)) + 1)
		} else {
			line = line + "Closed"
		}
		fmt.Println(any(line))
	}
}
