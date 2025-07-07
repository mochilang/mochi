//go:build ignore

package main

import "fmt"

func classify(n int) string {
	switch n {
	case 0:
		return "zero"
	case 1:
		return "one"
	default:
		return "many"
	}
}

func main() {
	x := 2
	var label string
	switch x {
	case 1:
		label = "one"
	case 2:
		label = "two"
	case 3:
		label = "three"
	default:
		label = "unknown"
	}
	fmt.Println(label)

	day := "sun"
	var mood string
	switch day {
	case "mon":
		mood = "tired"
	case "fri":
		mood = "excited"
	case "sun":
		mood = "relaxed"
	default:
		mood = "normal"
	}
	fmt.Println(mood)

	ok := true
	var status string
	switch ok {
	case true:
		status = "confirmed"
	case false:
		status = "denied"
	}
	fmt.Println(status)

	fmt.Println(classify(0))
	fmt.Println(classify(5))
}
