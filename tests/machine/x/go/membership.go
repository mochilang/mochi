//go:build ignore

package main

import (
    "fmt"
)

func contains(slice []int, v int) bool {
    for _, n := range slice {
        if n == v {
            return true
        }
    }
    return false
}

func main() {
    nums := []int{1, 2, 3}
    fmt.Println(contains(nums, 2))
    fmt.Println(contains(nums, 4))
}
