//go:build ignore

package main

import (
	"fmt"
	goffi "mochi/runtime/ffi/go"
)

func main() {
	fmt.Println(func() any { v, _ := goffi.Call("mochi/runtime/ffi/go/testpkg.FifteenPuzzleExample"); return v }())
}
