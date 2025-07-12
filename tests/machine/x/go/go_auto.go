//go:build ignore

package main

import (
	"fmt"
	goffi "mochi/runtime/ffi/go"
)

func main() {
	fmt.Println(func() any { v, _ := goffi.AttrAuto("mochi/runtime/ffi/go/testpkg", "Add", 2, 3); return v }())
	fmt.Println(func() any { v, _ := goffi.AttrAuto("mochi/runtime/ffi/go/testpkg", "Pi"); return v }())
	fmt.Println(func() any { v, _ := goffi.AttrAuto("mochi/runtime/ffi/go/testpkg", "Answer"); return v }())
}
