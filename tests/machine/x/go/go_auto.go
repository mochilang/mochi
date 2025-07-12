//go:build ignore

package main

import (
	"fmt"
	goffi "mochi/runtime/ffi/go"
	"reflect"
)

func main() {
	_print(func() any { v, _ := goffi.AttrAuto("mochi/runtime/ffi/go/testpkg", "Add", 2, 3); return v }())
	_print(func() any { v, _ := goffi.AttrAuto("mochi/runtime/ffi/go/testpkg", "Pi"); return v }())
	_print(func() any { v, _ := goffi.AttrAuto("mochi/runtime/ffi/go/testpkg", "Answer"); return v }())
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
