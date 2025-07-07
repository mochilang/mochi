//go:build ignore

package main

import (
	"log"

	ftncode "mochi/archived/x/fortran"
)

func main() {
	if err := ftncode.Ensure(); err != nil {
		log.Fatal(err)
	}
}
