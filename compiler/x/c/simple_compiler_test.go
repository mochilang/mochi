package ccode_test

import (
	"os"
	"path/filepath"
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/parser"
)

func TestSimpleCompiler(t *testing.T) {
	cases := []string{"print_hello", "unary_neg", "var_assignment"}
	root := filepath.Join("..", "..", "tests", "vm", "valid")
	outDir := filepath.Join("..", "..", "tests", "machine", "x", "c")
	for _, name := range cases {
		t.Run(name, func(t *testing.T) {
			src := filepath.Join(root, name+".mochi")
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			code, err := ccode.NewSimple().Compile(prog)
			if err != nil {
				t.Fatalf("compile: %v", err)
			}
			os.WriteFile(filepath.Join(outDir, name+".c"), code, 0644)
		})
	}
}
