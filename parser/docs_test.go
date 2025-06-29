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
		if stmt.Let != nil && stmt.Let.Name == "PI" && stmt.Let.Doc == "" {
			t.Errorf("missing doc for PI")
		}
		if stmt.Type != nil && stmt.Type.Name == "Person" {
			for _, m := range stmt.Type.Members {
				if m.Field != nil && m.Field.Name == "name" && m.Field.Doc == "" {
					t.Errorf("missing doc for field name")
				}
			}
		}
	}
	if !found {
		t.Fatalf("square function not found")
	}
}
