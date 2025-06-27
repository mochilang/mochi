//go:build ignore

package main

import (
	"log"

	ftncode "mochi/compile/x/fortran"
)

func main() {
	if err := ftncode.Ensure(); err != nil {
		log.Fatal(err)
	}
}
