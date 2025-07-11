//go:build ignore

package main

import (
	"fmt"
	goffi "mochi/runtime/ffi/go"
	"reflect"
)

func main() {
	fmt.Println(_sprint(func() any { v, _ := goffi.Call("mochi/runtime/ffi/go/testpkg.Add", 2, 3); return v }()))
	fmt.Println(_sprint(func() any { v, _ := goffi.Call("mochi/runtime/ffi/go/testpkg.Pi"); return v }()))
	fmt.Println(_sprint(func() any { v, _ := goffi.Call("mochi/runtime/ffi/go/testpkg.Answer"); return v }()))
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
