//go:build ignore

package main

import (
	"log"

	"mochi/tools/tcc"
)

func main() {
	if err := tcc.EnsureTCC(); err != nil {
		log.Fatal(err)
	}
}
