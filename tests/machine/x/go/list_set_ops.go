//go:build ignore

package main

import (
    "fmt"
)

func main() {
    fmt.Println([]int{1, 2} union []int{2, 3})
    fmt.Println([]int{1, 2, 3} except []int{2})
    fmt.Println([]int{1, 2, 3} intersect []int{2, 4})
    fmt.Println(len([]int{1, 2} union []int{2, 3}))
}
