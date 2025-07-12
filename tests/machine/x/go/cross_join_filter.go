//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var letters []string = []string{"A", "B"}
	_ = letters
	var pairs []Pairs = func() []Pairs {
		results := []Pairs{}
		for _, n := range nums {
			if (n % 2) == 0 {
				for _, l := range letters {
					results = append(results, Pairs{
						n,
						l,
					})
				}
			}
		}
		return results
	}()
	fmt.Println("--- Even pairs ---")
	for _, p := range pairs {
		_print(p.N, p.L)
	}
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
