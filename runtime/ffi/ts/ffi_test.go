package ts_test

import (
	"os/exec"
	"path/filepath"
	"testing"

	ts "mochi/runtime/ffi/ts"
)

func TestCall(t *testing.T) {
	r := ts.NewRuntime()
	r.Register("add", func(a, b int) int { return a + b })
	res, err := r.Call("add", 2, 3)
	if err != nil {
		t.Fatalf("call failed: %v", err)
	}
	if res.(int) != 5 {
		t.Fatalf("expected 5, got %v", res)
	}
}

func TestCallValue(t *testing.T) {
	r := ts.NewRuntime()
	r.Register("answer", 42)
	res, err := r.Call("answer")
	if err != nil {
		t.Fatalf("call value failed: %v", err)
	}
	if res.(int) != 42 {
		t.Fatalf("expected 42, got %v", res)
	}
}

func TestLoadModule(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}
	r := ts.NewRuntime()
	path := filepath.Join(".", "sample-module.mjs")
	if err := r.LoadModule(path); err != nil {
		t.Fatalf("load module failed: %v", err)
	}
	pi, err := r.Call("pi")
	if err != nil {
		t.Fatalf("call pi failed: %v", err)
	}
	if pi.(float64) != 3.14 {
		t.Fatalf("expected 3.14, got %v", pi)
	}
	sq, err := r.Call("square", 4)
	if err != nil {
		t.Fatalf("call square failed: %v", err)
	}
	if sq.(float64) != 16 {
		t.Fatalf("expected 16, got %v", sq)
	}
	pkgs := r.ListPackages()
	if len(pkgs) != 1 {
		t.Fatalf("expected 1 package, got %d", len(pkgs))
	}
	names := []string{pkgs[0].Exports[0].Name, pkgs[0].Exports[1].Name}
	got := make(map[string]bool)
	for _, n := range names {
		got[n] = true
	}
	if !got["pi"] || !got["square"] {
		t.Fatalf("exports incorrect: %+v", pkgs[0].Exports)
	}
}
