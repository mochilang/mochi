//go:build ignore

package main

import "fmt"

type Person struct {
	Name string
	Age  int
}

type Book struct {
	Title  string
	Author Person
}

func main() {
	book := Book{
		Title:  "Go",
		Author: Person{Name: "Bob", Age: 42},
	}
	fmt.Println(book.Author.Name)
}
