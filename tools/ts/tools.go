//go:build ignore

package main

import (
	"log"

	tscode "mochi/compile/ts"
)

func main() {
	if err := tscode.Ensure(); err != nil {
		log.Fatal(err)
	}
}
