//go:build ignore

package main

import (
	"fmt"
)

type Todo struct {
	Title string `json:"title"`
}

func main() {
	var todo Todo1 = Todo1{"hi"}
	_ = todo
	fmt.Println(todo.Title)
}
