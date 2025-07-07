//go:build ignore

package main

import (
	"log"

	cpp "mochi/archived/x/cpp"
)

func main() {
	if _, err := cpp.EnsureCPP(); err != nil {
		log.Fatal(err)
	}
	if err := cpp.EnsureClangFormat(); err != nil {
		log.Println(err)
	}
}
