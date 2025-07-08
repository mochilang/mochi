//go:build ignore

package main

import (
	"fmt"
)

type Todo struct {
	Title string
}

func main() {
	todo := &(Todo)(map[interface{}]interface{}{"title": "hi"})
	fmt.Println(todo.Title)
}
