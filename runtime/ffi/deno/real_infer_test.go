package deno_test

import (
	"os/exec"
	"testing"

	deno "mochi/runtime/ffi/deno"
)

func TestRealInfer(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}

	path := "https://deno.land/std@0.224.0/path/join.ts"
	info, err := deno.Infer(path)
	if err != nil {
		t.Fatalf("infer failed: %v", err)
	}

	if info.Path != path {
		t.Fatalf("expected path %s, got %s", path, info.Path)
	}

	foundJoin := false
	for _, fn := range info.Functions {
		if fn.Name == "join" {
			foundJoin = true
			if fn.Doc == "" {
				t.Fatalf("join doc missing")
			}
			if len(fn.Params) == 0 || fn.Params[0].Type == "" {
				t.Fatalf("join params incorrect: %+v", fn.Params)
			}
			if len(fn.Results) != 1 || fn.Results[0].Type != "string" {
				t.Fatalf("join results incorrect: %+v", fn.Results)
			}
		}
	}
	if !foundJoin {
		t.Fatalf("expected join function")
	}
}
