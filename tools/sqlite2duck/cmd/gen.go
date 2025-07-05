package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/tools/slt/logic"
	"mochi/tools/sqlite2duck"
)

func main() {
	flag.Parse()
	if flag.NArg() == 0 {
		fmt.Fprintln(os.Stderr, "usage: gen <file.test> [file.test ...]")
		os.Exit(1)
	}
	for _, in := range flag.Args() {
		cases, err := logic.ParseFile(in)
		if err != nil {
			fmt.Fprintf(os.Stderr, "parse %s: %v\n", in, err)
			os.Exit(1)
		}
		base := strings.TrimSuffix(filepath.Base(in), ".test")
		dir := filepath.Base(filepath.Dir(in))
		outPath := base + ".sql"
		if dir != "slt" && dir != "evidence" {
			outPath = dir + "_" + base + ".sql"
		}
		f, err := os.Create(outPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "create %s: %v\n", outPath, err)
			os.Exit(1)
		}
		for _, c := range cases {
			q := sqlite2duck.Convert(c.Query)
			if !strings.HasSuffix(q, ";") {
				q += ";"
			}
			fmt.Fprintln(f, q)
		}
		f.Close()
	}
}
