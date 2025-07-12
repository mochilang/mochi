//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

// line 3
func catalanRec(n int) int {
	if n == 0 {
		return 1
	}
	var t1 int = (2 * n)
	var t2 int = (t1 - 1)
	var t3 int = (2 * t2)
	var t5 int = (t3 * catalanRec((n - 1)))
	return int((float64(t5) / float64((n + 1))))
}

// line 12
func main() {
	for i := 1; i < 16; i++ {
		_print(fmt.Sprint(catalanRec(i)))
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
