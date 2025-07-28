package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	tsconv "mochi/tools/a2mochi/x/ts"
)

func main() {
	flag.Parse()
	for _, src := range flag.Args() {
		data, err := os.ReadFile(src)
		if err != nil {
			fmt.Fprintf(os.Stderr, "read %s: %v\n", src, err)
			continue
		}
		nodes, err := tsconv.Parse(string(data))
		if err != nil {
			fmt.Fprintf(os.Stderr, "parse %s: %v\n", src, err)
			continue
		}
		code, err := tsconv.ConvertSource(nodes, string(data))
		if err != nil {
			fmt.Fprintf(os.Stderr, "convert %s: %v\n", src, err)
			continue
		}
		out := filepath.Join(filepath.Dir(src), strings.TrimSuffix(filepath.Base(src), ".ts")+".mochi")
		ioutil.WriteFile(out, []byte(code), 0644)
	}
}
