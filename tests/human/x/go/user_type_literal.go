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
	var book Book = Book{Title: "Go", Author: Person{Name: "Bob", Age: 42}}
	_ = book
	fmt.Println(book.Author.Name)
}
