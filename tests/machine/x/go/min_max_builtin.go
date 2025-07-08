//go:build ignore

package main

import (
    "fmt"
)

func main() {
    nums := []int{3, 1, 4}
    fmt.Println(func() int { m := nums[0]; for _, v := range nums[1:] { if v < m { m = v } }; return m }())
    fmt.Println(func() int { m := nums[0]; for _, v := range nums[1:] { if v > m { m = v } }; return m }())
}
