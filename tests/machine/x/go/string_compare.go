//go:build ignore

package main

import (
	"fmt"
)

func main() {
	fmt.Println(("a" < "b"))
	fmt.Println(("a" <= "a"))
	fmt.Println(("b" > "a"))
	fmt.Println(("b" >= "b"))
}
