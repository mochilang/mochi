//go:build ignore

package main

import (
	"fmt"
)

// line 1
func bottles(n int) string {
	if n == 0 {
		return "No more bottles"
	}
	if n == 1 {
		return "1 bottle"
	}
	return fmt.Sprint(n) + " bottles"
}

// line 11
func main() {
	var i int = 99
	for {
		if !(i > 0) {
			break
		}
		fmt.Println(bottles(i) + " of beer on the wall")
		fmt.Println(bottles(i) + " of beer")
		fmt.Println("Take one down, pass it around")
		fmt.Println(bottles((i - 1)) + " of beer on the wall")
		i = (i - 1)
	}
}

func main() {
	main()
}
