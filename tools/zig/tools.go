//go:build ignore

package main

import (
	"log"

	zig "mochi/archived/x/zig"
)

func main() {
	if _, err := zig.EnsureZig(); err != nil {
		log.Fatal(err)
	}
}
