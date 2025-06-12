package python_test

import (
	"os/exec"
	"testing"

	"mochi/runtime/ffi/python"
)

func TestPackages(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	pkgs, err := python.Packages()
	if err != nil {
		t.Fatalf("packages failed: %v", err)
	}
	if len(pkgs) == 0 {
		t.Fatalf("expected at least one package")
	}
	found := false
	for _, p := range pkgs {
		if p.Name == "pip" {
			if p.Version == "" {
				t.Fatalf("pip version missing")
			}
			found = true
		}
	}
	if !found {
		t.Fatalf("pip package not found")
	}
}
