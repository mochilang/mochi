//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 6
func sinApprox(x float64) float64 {
	var term float64 = x
	var sum float64 = x
	var n int = 1
	for {
		if !(n <= 10) {
			break
		}
		var denom float64 = _cast[float64](((2 * n) * ((2 * n) + 1)))
		term = (((-term * x) * x) / denom)
		sum = (sum + term)
		n = (n + 1)
	}
	return sum
}

// line 19
func cosApprox(x float64) float64 {
	var term float64 = 1.0
	var sum float64 = 1.0
	var n int = 1
	for {
		if !(n <= 10) {
			break
		}
		var denom float64 = _cast[float64]((((2 * n) - 1) * (2 * n)))
		term = (((-term * x) * x) / denom)
		sum = (sum + term)
		n = (n + 1)
	}
	return sum
}

// line 32
func sqrtApprox(x float64) float64 {
	var guess float64 = x
	var i int = 0
	for {
		if !(i < 10) {
			break
		}
		guess = ((guess + (x / guess)) / 2.0)
		i = (i + 1)
	}
	return guess
}

func main() {
	var PI float64 = 3.141592653589793
	var L float64 = 10.0
	var G float64 = 9.81
	var dt float64 = 0.2
	var phi0 float64 = (PI / 4.0)
	var omega float64 = sqrtApprox((G / L))
	var t float64 = 0.0
	for step := 0; step < 10; step++ {
		var phi float64 = (phi0 * cosApprox((omega * t)))
		var pos int = _cast[int](((10.0 * sinApprox(phi)) + 0.5))
		fmt.Println(fmt.Sprint(pos))
		t = (t + dt)
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
