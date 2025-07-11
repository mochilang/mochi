//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

type Counter struct {
	N int `json:"n"`
}

// line 3
func inc(c *Counter) {
	c.N = (c.N + 1)
}

var c Counter

func main() {
	c = Counter{N: 0}
	inc(&c)
	fmt.Println(_sprint(c.N))
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
