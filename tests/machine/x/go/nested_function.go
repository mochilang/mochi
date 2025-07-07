//go:build ignore

    func inner(y int) int {
        return x + y
    }

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
