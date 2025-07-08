//go:build ignore

package main

import (
    "fmt"
)

func sum(nums []int) int {
    var s int
    for _, n := range nums {
        s += n
    }
    return s
}

func main() {
    fmt.Println(sum([]int{1, 2, 3}))
}
