package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	mlcode "mochi/compile/x/ocaml"
)

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}
	var report strings.Builder
	report.WriteString("# OCaml VM roundtrip test failures\n\n")
	for _, src := range files {
		if err := mlcode.RoundTripRun(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All OCaml VM roundtrip tests passed.\n")
	}
	os.WriteFile("compile/x/ocaml/ERRORS.md", []byte(report.String()), 0644)
}
