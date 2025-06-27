//go:build ignore

package main

import (
	"log"

	"mochi/tools/cosmo"
)

func main() {
	if err := cosmo.EnsureCosmo(); err != nil {
		log.Fatal(err)
	}
}
