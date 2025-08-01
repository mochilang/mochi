//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 22:51:15 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

var seededNow bool
var nowSeed int64

func init() {
	if s := os.Getenv("MOCHI_NOW_SEED"); s != "" {
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			nowSeed = v
			seededNow = true
		}
	}
}
func _now() int {
	if seededNow {
		nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
		return int(nowSeed)
	}
	return int(time.Now().UnixNano())
}

func _substr(s string, start, end int) string {
	r := []rune(s)
	if start < 0 {
		start = 0
	}
	if end > len(r) {
		end = len(r)
	}
	if start > len(r) {
		start = len(r)
	}
	if end < start {
		end = start
	}
	return string(r[start:end])
}

func pow10(n int) float64 {
	var r float64 = 1.0
	_ = r
	var i int = 0
	_ = i
	for i < n {
		r = (r * 10.0)
		i = (i + 1)
	}
	return r
}

func powf(base float64, exp float64) float64 {
	if exp == 0.5 {
		var guess float64 = base
		_ = guess
		var i int = 0
		_ = i
		for i < 20 {
			guess = ((guess + (base / guess)) / 2.0)
			i = (i + 1)
		}
		return guess
	}
	var result float64 = 1.0
	_ = result
	var n int = int(exp)
	_ = n
	var i int = 0
	_ = i
	for i < n {
		result = (result * base)
		i = (i + 1)
	}
	return result
}

func formatFloat(f float64, prec int) string {
	var scale float64 = pow10(prec)
	_ = scale
	var scaled float64 = ((f * scale) + 0.5)
	_ = scaled
	_ = scaled
	var n int = int(scaled)
	_ = n
	var digits string = fmt.Sprint(n)
	_ = digits
	for len(digits) <= prec {
		digits = ("0" + digits)
	}
	var intPart string = _substr(digits, 0, (len(digits) - prec))
	_ = intPart
	_ = intPart
	var fracPart string = _substr(digits, (len(digits) - prec), len(digits))
	_ = fracPart
	_ = fracPart
	return ((intPart + ".") + fracPart)
}

func padLeft(s string, w int) string {
	var res string = ""
	_ = res
	var n int = (w - len(s))
	_ = n
	for n > 0 {
		res = (res + " ")
		n = (n - 1)
	}
	return (res + s)
}

func rowString(row []float64) string {
	var s string = "["
	_ = s
	var i int = 0
	_ = i
	for i < len(row) {
		s = (s + padLeft(formatFloat(row[i], 3), 6))
		if i < (len(row) - 1) {
			s = (s + " ")
		}
		i = (i + 1)
	}
	return (s + "] ")
}

func printMatrix(heading string, m [][]float64) {
	fmt.Println(heading)
	var i int = 0
	_ = i
	for i < len(m) {
		fmt.Println(rowString(func(v any) []float64 {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]float64); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []float64{}
				}
				out := make([]float64, len(arr))
				for i, x := range arr {
					out[i] = x.(float64)
				}
				return out
			}
			return v.([]float64)
		}(m[i])))
		i = (i + 1)
	}
}

func elementWiseMM(m1 [][]float64, m2 [][]float64, f func(float64, float64) float64) [][]float64 {
	var z [][]float64 = [][]float64{}
	_ = z
	var r int = 0
	_ = r
	for r < len(m1) {
		var row []float64 = []float64{}
		_ = row
		var c int = 0
		_ = c
		for c < len(m1[r]) {
			row = append(row, f(m1[r][c], m2[r][c]))
			c = (c + 1)
		}
		z = append(z, row)
		r = (r + 1)
	}
	return z
}

func elementWiseMS(m [][]float64, s float64, f func(float64, float64) float64) [][]float64 {
	var z [][]float64 = [][]float64{}
	_ = z
	var r int = 0
	_ = r
	for r < len(m) {
		var row []float64 = []float64{}
		_ = row
		var c int = 0
		_ = c
		for c < len(m[r]) {
			row = append(row, f(m[r][c], s))
			c = (c + 1)
		}
		z = append(z, row)
		r = (r + 1)
	}
	return z
}

func add(a float64, b float64) float64 {
	return (a + b)
}

func sub(a float64, b float64) float64 {
	return (a - b)
}

func mul(a float64, b float64) float64 {
	return (a * b)
}

func div(a float64, b float64) float64 {
	return (a / b)
}

func exp(a float64, b float64) float64 {
	return powf(a, b)
}

func mochiMain() {
	var m1 [][]float64 = [][]float64{[]float64{3.0, 1.0, 4.0}, []float64{1.0, 5.0, 9.0}}
	_ = m1
	var m2 [][]float64 = [][]float64{[]float64{2.0, 7.0, 1.0}, []float64{8.0, 2.0, 8.0}}
	_ = m2
	printMatrix("m1:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1))
	printMatrix("m2:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m2))
	fmt.Println("")
	printMatrix("m1 + m2:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMM(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m2), add)))
	printMatrix("m1 - m2:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMM(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m2), sub)))
	printMatrix("m1 * m2:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMM(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m2), mul)))
	printMatrix("m1 / m2:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMM(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m2), div)))
	printMatrix("m1 ^ m2:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMM(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m2), exp)))
	fmt.Println("")
	var s float64 = 0.5
	_ = s
	fmt.Println(("s: " + fmt.Sprint(s)))
	printMatrix("m1 + s:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMS(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), s, add)))
	printMatrix("m1 - s:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMS(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), s, sub)))
	printMatrix("m1 * s:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMS(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), s, mul)))
	printMatrix("m1 / s:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMS(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), s, div)))
	printMatrix("m1 ^ s:", func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(elementWiseMS(func(v any) [][]float64 {
		if v == nil {
			return nil
		}
		if vv, ok := v.([][]float64); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return [][]float64{}
			}
			out := make([][]float64, len(arr))
			for i, x := range arr {
				out[i] = x.([]float64)
			}
			return out
		}
		return v.([][]float64)
	}(m1), s, exp)))
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
