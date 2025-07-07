//go:build ignore

package main

import (
    "fmt"
)

func avg(nums []int) int {
    if len(nums) == 0 {
        return 0
    }
    var sum int
    for _, n := range nums {
        sum += n
    }
    return sum / len(nums)
}

func main() {
fmt.Println(avg([]int{1, 2, 3}))
}
