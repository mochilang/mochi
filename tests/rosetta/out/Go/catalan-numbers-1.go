//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 3
func binom(n int, k int) int {
	if (k < 0) || (k > n) {
		return 0
	}
	var kk int = k
	if kk > (n - kk) {
		kk = (n - kk)
	}
	var res int = 1
	var i int = 0
	for {
		if !(i < kk) {
			break
		}
		res = (res * (n - i))
		i = (i + 1)
		res = int((float64(res) / float64(i)))
	}
	return res
}

// line 17
func catalan(n int) int {
	return int((float64(binom((2*n), n)) / float64((n + 1))))
}

// line 21
func main() {
	for i := 0; i < 15; i++ {
		_print(fmt.Sprint(catalan(i)))
	}
}

func main() {
	main()
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		rv := reflect.ValueOf(a)
		if a == nil || ((rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil()) {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
