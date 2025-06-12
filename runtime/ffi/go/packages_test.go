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
		t.Fatalf("no packages returned")
	}
	found := false
	for _, p := range pkgs {
		if p.Path == "fmt" {
			found = true
			if p.Doc == "" {
				t.Fatalf("expected doc for fmt")
			}
			break
		}
	}
	if !found {
		t.Fatalf("fmt package not found")
	}
}
