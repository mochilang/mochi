//go:build ignore
// +build ignore

package main

import "fmt"

func run(code string) {
    acc := 0
    for _, ch := range code {
        switch ch {
        case 'H':
            fmt.Println("Hello, world!")
        case 'Q':
            fmt.Println(code)
        case '9':
            fmt.Println("99 Bottles of Beer")
        case '+':
            acc++
        }
    }
    _ = acc
}

func main() {
    run("HQ9+")
}

