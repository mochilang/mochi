package goffi_test

import (
	"testing"

	goffi "mochi/runtime/ffi/go"
)

func TestPackages(t *testing.T) {
	pkgs, err := goffi.Packages()
	if err != nil {
		t.Fatalf("packages failed: %v", err)
	}
	if len(pkgs) == 0 {
		t.Fatalf("expected some packages")
	}
	found := false
	for _, p := range pkgs {
		if p.Path == "fmt" {
			found = true
			if p.Doc == "" {
				t.Fatalf("expected documentation for fmt")
			}
			break
		}
	}
	if !found {
		t.Fatalf("fmt not found in package list")
	}
}
