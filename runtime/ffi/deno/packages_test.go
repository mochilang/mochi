package deno_test

import (
	"os/exec"
	"testing"

	deno "mochi/runtime/ffi/deno"
)

func TestPackages(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}

	pkgs, err := deno.Packages()
	if err != nil {
		t.Fatalf("packages: %v", err)
	}
	if len(pkgs) == 0 {
		t.Fatalf("no packages found")
	}
	mathInfo, ok := pkgs["math.ts"]
	if !ok {
		t.Fatalf("expected math.ts package")
	}
	found := false
	for _, fn := range mathInfo.Functions {
		if fn.Name == "pow" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected pow function in math.ts")
	}
}
