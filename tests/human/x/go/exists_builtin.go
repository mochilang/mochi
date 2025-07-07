//go:build ignore

package main

import "fmt"

func main() {
	data := []int{1, 2}
	flag := false
	for _, x := range data {
		if x == 1 {
			flag = true
			break
		}
	}
	fmt.Println(flag)
}
