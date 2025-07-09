//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var letters []string = []string{"A", "B"}
	_ = letters
	type Pairs struct {
		N any `json:"n"`
		L any `json:"l"`
	}

	type Result struct {
		N int    `json:"n"`
		L string `json:"l"`
	}

	var pairs []Pairs = _cast[[]Pairs](func() []Result {
		_res := []Result{}
		for _, n := range nums {
			if (n % 2) == 0 {
				for _, l := range letters {
					_res = append(_res, Result{
						N: n,
						L: l,
					})
				}
			}
		}
		return _res
	}())
	fmt.Println("--- Even pairs ---")
	for _, p := range pairs {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(p.N), fmt.Sprint(p.L)}, " "), " "))
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
