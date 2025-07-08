//go:build ignore

package main

import (
    "fmt"
)

func min(nums []int) int {
    if len(nums) == 0 {
        panic("empty slice")
    }
    m := nums[0]
    for _, n := range nums[1:] {
        if n < m {
            m = n
        }
    }
    return m
}

func max(nums []int) int {
    if len(nums) == 0 {
        panic("empty slice")
    }
    m := nums[0]
    for _, n := range nums[1:] {
        if n > m {
            m = n
        }
    }
    return m
}

func main() {
    nums := []int{3, 1, 4}
    fmt.Println(min(nums))
    fmt.Println(max(nums))
}
