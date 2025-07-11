//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 3
func sinApprox(x float64) float64 {
	var term float64 = x
	var sum float64 = x
	var n int = 1
	for {
		if !(n <= 12) {
			break
		}
		var denom float64 = _cast[float64](((2 * n) * ((2 * n) + 1)))
		term = (((-term * x) * x) / denom)
		sum = (sum + term)
		n = (n + 1)
	}
	return sum
}

func main() {
	var PI float64 = 3.141592653589793
	var dt float64 = 0.01
	var s float64 = 0.0
	var t1 float64 = 0.0
	var k1 float64 = sinApprox(0.0)
	var i int = 1
	for {
		if !(i <= 200) {
			break
		}
		var t2 float64 = ((_cast[float64](i)) * dt)
		var k2 float64 = sinApprox((t2 * PI))
		s = (s + (((k1 + k2) * 0.5) * (t2 - t1)))
		t1 = t2
		k1 = k2
		i = (i + 1)
	}
	var i2 int = 1
	for {
		if !(i2 <= 50) {
			break
		}
		var t2 float64 = (2.0 + ((_cast[float64](i2)) * dt))
		var k2 float64 = 0.0
		s = (s + (((k1 + k2) * 0.5) * (t2 - t1)))
		t1 = t2
		k1 = k2
		i2 = (i2 + 1)
	}
	fmt.Println(s)
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
