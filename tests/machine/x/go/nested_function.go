    func inner(y int) int {
        return x + y
    }
//go:build ignore

package main

import (
    "fmt"
)

func outer(x int) int {
    return inner(5)
}


func main() {
    fmt.Println(outer(3))
}
