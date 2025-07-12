//go:build ignore

package main

import (
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

type Book struct {
	Title  string `json:"title"`
	Author Person `json:"author"`
}

func main() {
	var book Book = Book{"Go", Person{"Bob", 42}}
	_ = book
	fmt.Println(book.Author.Name)
}
