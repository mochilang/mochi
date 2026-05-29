package build

import (
	"strings"
	"testing"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/ruby/lower"
	"mochi/types"
)

// TestPhase0LowerHello exercises the parse -> typecheck -> aotir lower ->
// ruby lower path on the canonical hello fixture. It does not require Ruby
// on PATH; it only validates that the rtree.SourceFile renders the expected
// Ruby source.
func TestPhase0LowerHello(t *testing.T) {
	srcPath := repoRootForTest(t) + "/examples/v0.1/hello.mochi"

	ast, err := parser.Parse(srcPath)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		t.Fatalf("typecheck: %v", errs[0])
	}
	prog, err := clower.Lower(ast)
	if err != nil {
		t.Fatalf("aotir lower: %v", err)
	}
	sf, err := lower.Lower(prog, "hello", "Hello")
	if err != nil {
		t.Fatalf("ruby lower: %v", err)
	}
	got := sf.RubySource()
	expected := []string{
		"# frozen_string_literal: true",
		`require "mochi/runtime"`,
		"module Hello",
		"module Main",
		"def self.run(argv)",
		`puts "Hello, world"`,
		"end",
	}
	for _, want := range expected {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in rendered source:\n%s", want, got)
		}
	}
}
