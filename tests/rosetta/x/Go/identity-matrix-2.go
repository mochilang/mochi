//go:build ignore
// +build ignore

package main

import (
    "fmt"

    mat "github.com/skelterjohn/go.matrix"
)

func main() {
    fmt.Println(mat.Eye(3))
}
