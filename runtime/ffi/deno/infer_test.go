package deno_test

import (
	"os/exec"
	"path/filepath"
	"testing"

	deno "mochi/runtime/ffi/deno"
)

func TestInfer(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}
	p, err := filepath.Abs("runtime/ffi/deno/math.ts")
	if err != nil {
		t.Fatalf("abs failed: %v", err)
	}
	info, err := deno.Infer("file://" + p)
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	foundPow := false
	for _, f := range info.Functions {
		if f.Name == "pow" {
			foundPow = true
			break
		}
	}
	if !foundPow {
		t.Fatalf("expected pow function in inference results")
	}

	foundPI := false
	for _, c := range info.Consts {
		if c.Name == "PI" {
			foundPI = true
			break
		}
	}
	if !foundPI {
		t.Fatalf("expected PI constant")
	}
}
