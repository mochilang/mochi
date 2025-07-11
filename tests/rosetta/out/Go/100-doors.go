//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
)

func main() {
	var doors []any = []any{}
	for i := 0; i < 100; i++ {
		doors = append(doors, false)
	}
	for pass := 1; pass < 101; pass++ {
		var idx int = (pass - 1)
		for {
			if !(idx < 100) {
				break
			}
			doors[idx] = !doors[idx]
			idx = (idx + pass)
		}
	}
	for row := 0; row < 10; row++ {
		var line string = ""
		for col := 0; col < 10; col++ {
			var idx int = ((row * 10) + col)
			if _exists(doors[idx]) {
				line = line + "1"
			} else {
				line = line + "0"
			}
			if col < 9 {
				line = line + " "
			}
		}
		fmt.Println(line)
	}
}

func _exists(v any) bool {
	if g, ok := v.(*data.Group); ok {
		return len(g.Items) > 0
	}
	switch s := v.(type) {
	case []any:
		return len(s) > 0
	case []int:
		return len(s) > 0
	case []float64:
		return len(s) > 0
	case []string:
		return len(s) > 0
	case []bool:
		return len(s) > 0
	case []map[string]any:
		return len(s) > 0
	case map[string]any:
		return len(s) > 0
	case string:
		return len([]rune(s)) > 0
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Slice || rv.Kind() == reflect.Array {
		return rv.Len() > 0
	}
	return false
}
