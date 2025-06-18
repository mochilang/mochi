package main

import (
	"fmt"
	"mochi/compile/ts"
)

func main() {
	if err := tscode.EnsureDeno(); err != nil {
		fmt.Println("failed to install Deno:", err)
	}
}
