//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

type Todo struct {
	Title string `json:"title"`
}

func main() {
	type Todo1 struct {
		Title string `json:"title"`
	}

	var todo Todo1 = Todo1{Title: "hi"}
	_ = todo
	fmt.Println(_sprint(todo.Title))
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
