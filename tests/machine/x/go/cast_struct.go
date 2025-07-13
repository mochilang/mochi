//go:build ignore

package main

import (
	"fmt"
)

type Todo struct {
	Title string `json:"title"`
}

func main() {
	todo := Todo{"hi"}
	_ = todo
	fmt.Println(todo.Title)
}
