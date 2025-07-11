//go:build ignore

package main

import (
	"encoding/json"
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
	fmt.Println(_fmt(c.N))
}

func _fmt(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return "<nil>"
		}
		v = rv.Elem().Interface()
		rv = reflect.ValueOf(v)
	}
	if rv.Kind() == reflect.Struct {
		if rv.IsZero() {
			return "<nil>"
		}
		b, _ := json.Marshal(v)
		var m map[string]any
		_ = json.Unmarshal(b, &m)
		return fmt.Sprint(m)
	}
	return fmt.Sprint(v)
}
