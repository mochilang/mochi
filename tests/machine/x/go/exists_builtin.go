//go:build ignore

package main

import (
    "fmt"
)

func main() {
    data := []int{1, 2}
    flag := func() bool {
        for _, x := range data {
            if x == 1 {
                return true
            }
        }
        return false
    }()
    fmt.Println(flag)
}
