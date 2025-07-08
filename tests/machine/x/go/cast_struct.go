//go:build ignore

package main

import (
    "fmt"
)

type Todo struct {
    Title string
}


func main() {
    todo := &Todo{Title: "hi"}
    fmt.Println(todo.Title)
}
