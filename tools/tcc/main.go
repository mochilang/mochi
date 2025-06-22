//go:build ignore

package main

import (
	"fmt"

	"mochi/tools/tcc"
)

func main() {
	res, err := tcc.CompileAndRun(`
int square(int x) { return x * x; }
`)
	if err != nil {
		panic(err)
	}
	fmt.Println(res)
}
