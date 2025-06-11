package parser_test

import (
	"os"
	"testing"

	"mochi/parser"
)

func TestParser_DocComments(t *testing.T) {
	src, err := os.ReadFile("../examples/v0.7/docs.mochi")
	if err != nil {
		t.Fatal(err)
	}
	prog, err := parser.ParseString(string(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if prog.PackageDoc == "" {
		t.Fatalf("expected package doc")
	}
	found := false
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil && stmt.Fun.Name == "square" {
			if stmt.Fun.Doc == "" {
				t.Errorf("missing doc for square")
			}
			found = true
		}
	}
	if !found {
		t.Fatalf("square function not found")
	}
}
