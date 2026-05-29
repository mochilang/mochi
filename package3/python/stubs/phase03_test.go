package stubs

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase3StubIngest is the umbrella sentinel for MEP-71 Phase 3. It exercises
// the 4-tier discovery in cooperation with the .pyi reader, so the closeout
// gate can run a single `go test -run TestPhase3StubIngest`.
func TestPhase3StubIngest(t *testing.T) {
	t.Run("inline_tier_wins", func(t *testing.T) {
		dir := t.TempDir()
		pkg := filepath.Join(dir, "pkg")
		sib := filepath.Join(dir, "pkg-stubs")
		for _, d := range []string{pkg, sib} {
			if err := os.MkdirAll(d, 0o755); err != nil {
				t.Fatal(err)
			}
		}
		if err := os.WriteFile(filepath.Join(pkg, "py.typed"), nil, 0o644); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(pkg, "__init__.pyi"), nil, 0o644); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(sib, "__init__.pyi"), nil, 0o644); err != nil {
			t.Fatal(err)
		}
		d := &Discovery{SitePackages: dir}
		src, err := d.Resolve("pkg")
		if err != nil {
			t.Fatal(err)
		}
		if src.Tier != TierInline {
			t.Errorf("Tier = %v, want TierInline", src.Tier)
		}
	})

	t.Run("sibling_stubs_tier", func(t *testing.T) {
		dir := t.TempDir()
		sib := filepath.Join(dir, "pkg-stubs")
		if err := os.MkdirAll(sib, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(sib, "__init__.pyi"), nil, 0o644); err != nil {
			t.Fatal(err)
		}
		d := &Discovery{SitePackages: dir}
		src, err := d.Resolve("pkg")
		if err != nil {
			t.Fatal(err)
		}
		if src.Tier != TierSiblingStubs {
			t.Errorf("Tier = %v, want TierSiblingStubs", src.Tier)
		}
	})

	t.Run("typeshed_tier", func(t *testing.T) {
		dir := t.TempDir()
		tsRoot := t.TempDir()
		stdlibPkg := filepath.Join(tsRoot, "stdlib", "pkg")
		if err := os.MkdirAll(stdlibPkg, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(stdlibPkg, "__init__.pyi"), nil, 0o644); err != nil {
			t.Fatal(err)
		}
		ts, err := NewTypeshed(tsRoot, "deadbeef")
		if err != nil {
			t.Fatal(err)
		}
		d := &Discovery{SitePackages: dir, Typeshed: ts}
		src, err := d.Resolve("pkg")
		if err != nil {
			t.Fatal(err)
		}
		if src.Tier != TierTypeshed {
			t.Errorf("Tier = %v, want TierTypeshed", src.Tier)
		}
	})

	t.Run("stubgen_fallback", func(t *testing.T) {
		dir := t.TempDir()
		cacheDir := t.TempDir()
		d := &Discovery{
			SitePackages: dir,
			Stubgen:      &FakeStubgen{CacheDir: cacheDir, Body: []byte("def f() -> int: ...\n")},
			AllowStubgen: true,
		}
		src, err := d.Resolve("pkg")
		if err != nil {
			t.Fatal(err)
		}
		if src.Tier != TierStubgen {
			t.Errorf("Tier = %v, want TierStubgen", src.Tier)
		}
		if !src.Partial {
			t.Error("stubgen output must be marked Partial")
		}
		// Round-trip the body through ParsePYI to confirm the end-to-end ingest works.
		body, err := os.ReadFile(filepath.Join(src.RootDir, "pkg.pyi"))
		if err != nil {
			t.Fatal(err)
		}
		m, err := ParsePYI(string(body))
		if err != nil {
			t.Fatal(err)
		}
		if len(m.Functions) != 1 || m.Functions[0].Name != "f" || m.Functions[0].ReturnType != "int" {
			t.Errorf("parsed surface = %+v", m)
		}
	})

	t.Run("pyi_round_trip", func(t *testing.T) {
		src := `from typing import List, Optional

def echo(x: int) -> int: ...

class Greeter:
    name: str
    def hello(self, who: Optional[str] = None) -> str: ...

Vector = List[float]
MAX: int = 100
`
		m, err := ParsePYI(src)
		if err != nil {
			t.Fatal(err)
		}
		if len(m.Imports) != 1 || m.Imports[0].Module != "typing" {
			t.Errorf("imports = %+v", m.Imports)
		}
		if len(m.Functions) != 1 || m.Functions[0].Name != "echo" {
			t.Errorf("funcs = %+v", m.Functions)
		}
		if len(m.Classes) != 1 || m.Classes[0].Name != "Greeter" {
			t.Errorf("classes = %+v", m.Classes)
		}
		if len(m.Classes[0].Methods) != 1 || m.Classes[0].Methods[0].ReturnType != "str" {
			t.Errorf("Greeter methods = %+v", m.Classes[0].Methods)
		}
		if len(m.Aliases) != 1 || m.Aliases[0].Name != "Vector" {
			t.Errorf("aliases = %+v", m.Aliases)
		}
		if len(m.Constants) != 1 || m.Constants[0].Name != "MAX" || m.Constants[0].Default != "100" {
			t.Errorf("constants = %+v", m.Constants)
		}
	})
}
