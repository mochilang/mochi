package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"mochi/exp/compiler/chibigo/rewrite"
)

func main() {
	bin := flag.String("chibicc", defaultChibiccPath(), "path to chibicc binary")
	flag.Parse()

	compiler, err := chibigo.New(*bin)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := compiler.Compile(flag.Args()...); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func defaultChibiccPath() string {
	wd, err := os.Getwd()
	if err != nil {
		return "exp/compiler/chibicc/chibicc"
	}
	return filepath.Join(wd, "exp", "compiler", "chibicc", "chibicc")
}
