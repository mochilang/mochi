//go:build ignore
// +build ignore

package main

import (
	"io"
	"os"
)

func main() {
	io.Copy(os.Stdout, os.Stdin)
}
