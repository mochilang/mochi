//go:build ignore

package main

import "fmt"

func main() {
	matrix := [][]int{{1, 2}, {3, 4}}
	matrix[1][0] = 5
	fmt.Println(matrix[1][0])
}
