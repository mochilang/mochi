//go:build ignore

package main

import (
	"fmt"
	"reflect"
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
	fmt.Println(_sprint(book.Author.Name))
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
