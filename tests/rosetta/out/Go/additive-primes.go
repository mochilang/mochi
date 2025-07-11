//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 1
func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	if (n % 2) == 0 {
		return (n == 2)
	}
	if (n % 3) == 0 {
		return (n == 3)
	}
	var d int = 5
	for {
		if !((d * d) <= n) {
			break
		}
		if (n % d) == 0 {
			return false
		}
		d = (d + 2)
		if (n % d) == 0 {
			return false
		}
		d = (d + 4)
	}
	return true
}

// line 15
func sumDigits(n int) int {
	var s int = 0
	var x int = n
	for {
		if !(x > 0) {
			break
		}
		s = (s + (x % 10))
		x = _cast[int]((float64(x) / float64(10)))
	}
	return s
}

// line 25
func pad(n int) string {
	if n < 10 {
		return "  " + fmt.Sprint(n)
	}
	if n < 100 {
		return " " + fmt.Sprint(n)
	}
	return fmt.Sprint(n)
}

// line 31
func main() {
	fmt.Println("Additive primes less than 500:")
	var count int = 0
	var line string = ""
	var lineCount int = 0
	var i int = 2
	for {
		if !(i < 500) {
			break
		}
		if isPrime(i) && isPrime(sumDigits(i)) {
			count = (count + 1)
			line = line + pad(i) + "  "
			lineCount = (lineCount + 1)
			if lineCount == 10 {
				fmt.Println(_sliceString(line, 0, (len(line) - 2)))
				line = ""
				lineCount = 0
			}
		}
		if i > 2 {
			i = (i + 2)
		} else {
			i = (i + 1)
		}
	}
	if lineCount > 0 {
		fmt.Println(_sliceString(line, 0, (len(line) - 2)))
	}
	fmt.Println(fmt.Sprint(count) + " additive primes found.")
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

func _sliceString(s string, i, j int) string {
	start := i
	end := j
	n := len([]rune(s))
	if start < 0 {
		start += n
	}
	if end < 0 {
		end += n
	}
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if end < start {
		end = start
	}
	return string([]rune(s)[start:end])
}
