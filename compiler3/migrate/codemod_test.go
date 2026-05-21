package migrate

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestScanCallSitesFindsLiteralCallee asserts the scanner picks up the
// canonical legacy emit shape: `goffi.Call("strings.ToUpper", x)`.
func TestScanCallSitesFindsLiteralCallee(t *testing.T) {
	dir := t.TempDir()
	src := `package x

import goffi "mochi/runtime/ffi/go"

func F(a string) any {
	v, _ := goffi.Call("strings.ToUpper", a)
	return v
}
`
	path := filepath.Join(dir, "x.go")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	sites, err := ScanCallSites(dir)
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(sites) != 1 {
		t.Fatalf("want 1 site, got %d: %+v", len(sites), sites)
	}
	s := sites[0]
	if s.Pkg != "strings" || s.Func != "ToUpper" {
		t.Errorf("pkg/func mismatch: %+v", s)
	}
	if s.NumArgs != 1 {
		t.Errorf("want 1 arg, got %d", s.NumArgs)
	}
}

// TestScanCallSitesNonLiteralCallee asserts a non-literal callee still
// records the call site so the migrator can flag it for manual review.
func TestScanCallSitesNonLiteralCallee(t *testing.T) {
	dir := t.TempDir()
	src := `package x

import goffi "mochi/runtime/ffi/go"

func F(name string, a string) any {
	v, _ := goffi.Call(name, a)
	return v
}
`
	path := filepath.Join(dir, "x.go")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	sites, err := ScanCallSites(dir)
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(sites) != 1 {
		t.Fatalf("want 1 site, got %d", len(sites))
	}
	if sites[0].Pkg != "" || sites[0].Func != "" {
		t.Errorf("non-literal callee should leave Pkg/Func empty: %+v", sites[0])
	}
}

// TestSiteReportLine asserts the report includes file:line, pkg.func,
// and arg count so a human reader can act on it.
func TestSiteReportLine(t *testing.T) {
	r := SiteReport([]CallSite{{File: "a.go", Line: 7, Pkg: "strings", Func: "ToUpper", NumArgs: 1}})
	if !strings.Contains(r, "a.go:7") || !strings.Contains(r, "strings.ToUpper") || !strings.Contains(r, "args=1") {
		t.Errorf("report missing required fields:\n%s", r)
	}
}

// TestScanSkipsTestFiles asserts the scanner does not report sites in
// *_test.go (codemod target is emitted production code).
func TestScanSkipsTestFiles(t *testing.T) {
	dir := t.TempDir()
	src := `package x

import goffi "mochi/runtime/ffi/go"

func F(a string) any {
	v, _ := goffi.Call("strings.ToUpper", a)
	return v
}
`
	if err := os.WriteFile(filepath.Join(dir, "x_test.go"), []byte(src), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	sites, err := ScanCallSites(dir)
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(sites) != 0 {
		t.Errorf("test file should be skipped, got %+v", sites)
	}
}
