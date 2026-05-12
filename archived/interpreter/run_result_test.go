//go:build slow

package interpreter

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

func TestRunResult_SingleExpr(t *testing.T) {
	prog, err := parser.ParseString("1 + 4")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	interp := New(prog, env, "")
	v, err := interp.RunResult()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if v != 5 {
		t.Fatalf("want 5, got %v", v)
	}
}

func TestRunResult_LastExpr(t *testing.T) {
	prog, err := parser.ParseString("let x = 3\nx + 2")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	interp := New(prog, env, "")
	v, err := interp.RunResult()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if v != 5 {
		t.Fatalf("want 5, got %v", v)
	}
}

func TestRunResult_NoExpr(t *testing.T) {
	prog, err := parser.ParseString("let x = 3")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	interp := New(prog, env, "")
	v, err := interp.RunResult()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if v != nil {
		t.Fatalf("want nil, got %v", v)
	}
}
