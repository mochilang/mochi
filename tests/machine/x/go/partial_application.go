//go:build ignore

package main

import (
	"fmt"
)

// line 1
func add(a int, b int) int {
	return (a + b)
}

func main() {
	var add5 func(int) int = _cast[func(int) int](func(p0 int) int { return add(5, p0) })
	fmt.Println(add5(3))
}

func _cast[T any](v any) T {
	return v.(T)
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
