//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 4
func kPrime(n int, k int) bool {
	var nf int = 0
	var i int = 2
	for {
		if !(i <= n) {
			break
		}
		for {
			if !((n % i) == 0) {
				break
			}
			if nf == k {
				return false
			}
			nf = (nf + 1)
			n = int(int(_cast[int]((float64(n) / float64(i)))))
		}
		i = (i + 1)
	}
	return (nf == k)
}

// line 18
func gen(k int, count int) []int {
	var r []int = []int{}
	var n int = 2
	for {
		if !(len(r) < count) {
			break
		}
		if kPrime(n, k) {
			r = append(_convSlice[int, any](r), n)
		}
		n = (n + 1)
	}
	return r
}

// line 28
func main() {
	var k int = 1
	for {
		if !(k <= 5) {
			break
		}
		fmt.Println(fmt.Sprint(k) + " " + fmt.Sprint(gen(k, 10)))
		k = (k + 1)
	}
}

func main() {
	main()
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

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
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
