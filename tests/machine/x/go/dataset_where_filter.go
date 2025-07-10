//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strconv"
	"strings"
)

func main() {
	var people []map[string]any = []map[string]any{
		map[string]any{"name": "Alice", "age": 30},
		map[string]any{"name": "Bob", "age": 15},
		map[string]any{"name": "Charlie", "age": 65},
		map[string]any{"name": "Diana", "age": 45},
	}
	var adults []map[string]any = func() []map[string]any {
		_res := []map[string]any{}
		for _, person := range people {
			if _cast[int](person["age"]) >= 18 {
				if _cast[int](person["age"]) >= 18 {
					_res = append(_res, map[string]any{
						"name":      person["name"],
						"age":       person["age"],
						"is_senior": (_cast[int](person["age"]) >= 60),
					})
				}
			}
		}
		return _res
	}()
	fmt.Println("--- Adults ---")
	for _, person := range adults {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(person["name"]), fmt.Sprint("is"), fmt.Sprint(person["age"]), fmt.Sprint(func() string {
			if _exists(person["is_senior"]) {
				return " (senior)"
			} else {
				return ""
			}
		}())}, " "), " "))
	}
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		case string:
			n, _ := strconv.Atoi(vv)
			return any(n).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
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
