//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 1
func poly(p int) string {
	var s string = ""
	var coef int = 1
	var i int = p
	if coef != 1 {
		s = s + fmt.Sprint(coef)
	}
	for {
		if !(i > 0) {
			break
		}
		s = s + "x"
		if i != 1 {
			s = s + "^" + fmt.Sprint(i)
		}
		coef = _cast[int]((float64((coef * i)) / float64(((p - i) + 1))))
		var d int = coef
		if ((p - (i - 1)) % 2) == 1 {
			d = -d
		}
		if d < 0 {
			s = s + " - " + fmt.Sprint(-d)
		} else {
			s = s + " + " + fmt.Sprint(d)
		}
		i = (i - 1)
	}
	if s == "" {
		s = "1"
	}
	return s
}

// line 31
func aks(n int) bool {
	if n < 2 {
		return false
	}
	var c int = n
	var i int = 1
	for {
		if !(i < n) {
			break
		}
		if (c % n) != 0 {
			return false
		}
		c = _cast[int]((float64((c * (n - i))) / float64((i + 1))))
		i = (i + 1)
	}
	return true
}

// line 43
func main() {
	var p int = 0
	for {
		if !(p <= 7) {
			break
		}
		fmt.Println(fmt.Sprint(p) + ":  " + poly(p))
		p = (p + 1)
	}
	var first bool = true
	p = 2
	var line string = ""
	for {
		if !(p < 50) {
			break
		}
		if aks(p) {
			if first {
				line = line + fmt.Sprint(p)
				first = false
			} else {
				line = line + " " + fmt.Sprint(p)
			}
		}
		p = (p + 1)
	}
	fmt.Println(line)
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
