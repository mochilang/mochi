package stubs

import (
	"os"
	"path/filepath"
	"testing"
)

func TestTierString(t *testing.T) {
	cases := []struct {
		tier Tier
		want string
	}{
		{TierInline, "inline"},
		{TierSiblingStubs, "sibling-stubs"},
		{TierTypeshed, "typeshed"},
		{TierStubgen, "stubgen"},
		{TierUnknown, "unknown"},
		{Tier(99), "unknown"},
	}
	for _, c := range cases {
		if got := c.tier.String(); got != c.want {
			t.Errorf("Tier(%d).String() = %q, want %q", c.tier, got, c.want)
		}
	}
}

func TestResolveEmptyName(t *testing.T) {
	d := &Discovery{}
	if _, err := d.Resolve(""); err == nil {
		t.Fatal("expected error for empty name")
	}
}

func TestResolveInlineFull(t *testing.T) {
	dir := t.TempDir()
	pkg := filepath.Join(dir, "mypkg")
	if err := os.MkdirAll(pkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkg, "py.typed"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkg, "__init__.pyi"), []byte("X: int\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	d := &Discovery{SitePackages: dir}
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierInline {
		t.Errorf("Tier = %v, want TierInline", src.Tier)
	}
	if src.Partial {
		t.Error("Partial should be false for empty py.typed")
	}
	if len(src.Files) == 0 {
		t.Error("expected at least one file")
	}
}

func TestResolveInlinePartial(t *testing.T) {
	dir := t.TempDir()
	pkg := filepath.Join(dir, "mypkg")
	if err := os.MkdirAll(pkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkg, "py.typed"), []byte("partial\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkg, "__init__.pyi"), []byte("X: int\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	d := &Discovery{SitePackages: dir}
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if !src.Partial {
		t.Error("Partial should be true for partial py.typed")
	}
}

func TestResolveSiblingStubs(t *testing.T) {
	dir := t.TempDir()
	stubs := filepath.Join(dir, "mypkg-stubs")
	if err := os.MkdirAll(stubs, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(stubs, "__init__.pyi"), []byte("def f() -> int: ...\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	d := &Discovery{SitePackages: dir}
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierSiblingStubs {
		t.Errorf("Tier = %v, want TierSiblingStubs", src.Tier)
	}
	if src.Partial {
		t.Error("sibling stubs should not be marked partial")
	}
}

func TestResolveInlineBeatsSibling(t *testing.T) {
	dir := t.TempDir()
	pkg := filepath.Join(dir, "mypkg")
	sib := filepath.Join(dir, "mypkg-stubs")
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
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierInline {
		t.Errorf("Tier = %v, want TierInline (inline must beat sibling)", src.Tier)
	}
}

func TestResolveSiblingBeatsTypeshed(t *testing.T) {
	dir := t.TempDir()
	tsRoot := t.TempDir()
	// sibling stubs
	sib := filepath.Join(dir, "mypkg-stubs")
	if err := os.MkdirAll(sib, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(sib, "__init__.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	// typeshed third-party entry
	tsPkg := filepath.Join(tsRoot, "stubs", "mypkg", "mypkg")
	if err := os.MkdirAll(tsPkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(tsRoot, "stdlib"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tsPkg, "__init__.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	ts, err := NewTypeshed(tsRoot, "deadbeef")
	if err != nil {
		t.Fatal(err)
	}
	d := &Discovery{SitePackages: dir, Typeshed: ts}
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierSiblingStubs {
		t.Errorf("Tier = %v, want TierSiblingStubs", src.Tier)
	}
}

func TestResolveTypeshedBeatsStubgen(t *testing.T) {
	dir := t.TempDir()
	tsRoot := t.TempDir()
	cacheDir := t.TempDir()
	tsPkg := filepath.Join(tsRoot, "stdlib", "mypkg")
	if err := os.MkdirAll(tsPkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tsPkg, "__init__.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	ts, err := NewTypeshed(tsRoot, "")
	if err != nil {
		t.Fatal(err)
	}
	d := &Discovery{
		SitePackages: dir,
		Typeshed:     ts,
		Stubgen:      &FakeStubgen{CacheDir: cacheDir, Body: []byte("def f(): ...\n")},
		AllowStubgen: true,
	}
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierTypeshed {
		t.Errorf("Tier = %v, want TierTypeshed", src.Tier)
	}
}

func TestResolveStubgenFallback(t *testing.T) {
	dir := t.TempDir()
	cacheDir := t.TempDir()
	d := &Discovery{
		SitePackages: dir,
		Stubgen:      &FakeStubgen{CacheDir: cacheDir, Body: []byte("def f(): ...\n")},
		AllowStubgen: true,
	}
	src, err := d.Resolve("mypkg")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierStubgen {
		t.Errorf("Tier = %v, want TierStubgen", src.Tier)
	}
	if !src.Partial {
		t.Error("stubgen output must be marked Partial")
	}
}

func TestResolveStubgenDisabled(t *testing.T) {
	dir := t.TempDir()
	cacheDir := t.TempDir()
	d := &Discovery{
		SitePackages: dir,
		Stubgen:      &FakeStubgen{CacheDir: cacheDir, Body: []byte("x")},
		AllowStubgen: false,
	}
	if _, err := d.Resolve("mypkg"); err == nil {
		t.Fatal("expected not-found error when AllowStubgen is false")
	}
}

func TestResolveNotFound(t *testing.T) {
	d := &Discovery{SitePackages: t.TempDir()}
	if _, err := d.Resolve("mypkg"); err == nil {
		t.Fatal("expected not-found error")
	}
}

func TestResolveHyphenatedNameMapsToUnderscore(t *testing.T) {
	dir := t.TempDir()
	// PEP 503 name `flask-sqlalchemy` maps to module `flask_sqlalchemy`.
	pkg := filepath.Join(dir, "flask_sqlalchemy")
	if err := os.MkdirAll(pkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkg, "py.typed"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	d := &Discovery{SitePackages: dir}
	src, err := d.Resolve("flask-sqlalchemy")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierInline {
		t.Errorf("Tier = %v", src.Tier)
	}
}

func TestWalkStubFilesSorted(t *testing.T) {
	dir := t.TempDir()
	for _, f := range []string{"z.pyi", "a.pyi", "sub/m.pyi"} {
		full := filepath.Join(dir, f)
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(full, nil, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	got, err := walkStubFiles(dir)
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"a.pyi", filepath.Join("sub", "m.pyi"), "z.pyi"}
	if len(got) != len(want) {
		t.Fatalf("got %v, want %v", got, want)
	}
	for i, g := range got {
		if g != want[i] {
			t.Errorf("file[%d] = %q, want %q", i, g, want[i])
		}
	}
}

func TestWalkStubFilesPyFallback(t *testing.T) {
	dir := t.TempDir()
	// shadowed.py is shadowed by shadowed.pyi
	if err := os.WriteFile(filepath.Join(dir, "shadowed.py"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "shadowed.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	// onlypy.py has no .pyi twin so it should appear.
	if err := os.WriteFile(filepath.Join(dir, "onlypy.py"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	got, err := walkStubFiles(dir)
	if err != nil {
		t.Fatal(err)
	}
	hasPyi := false
	hasOnlyPy := false
	hasShadowedPy := false
	for _, g := range got {
		switch g {
		case "shadowed.pyi":
			hasPyi = true
		case "onlypy.py":
			hasOnlyPy = true
		case "shadowed.py":
			hasShadowedPy = true
		}
	}
	if !hasPyi {
		t.Error("shadowed.pyi missing")
	}
	if !hasOnlyPy {
		t.Error("onlypy.py missing")
	}
	if hasShadowedPy {
		t.Error("shadowed.py should be shadowed by .pyi twin")
	}
}

func TestReadPartialMarker(t *testing.T) {
	dir := t.TempDir()
	cases := []struct {
		body string
		want bool
	}{
		{"", false},
		{"partial", true},
		{"partial\n", true},
		{"  partial  ", true},
		{"full", false},
		{"PARTIAL", false},
	}
	for i, c := range cases {
		p := filepath.Join(dir, "f.txt")
		if err := os.WriteFile(p, []byte(c.body), 0o644); err != nil {
			t.Fatal(err)
		}
		got, err := readPartialMarker(p)
		if err != nil {
			t.Fatal(err)
		}
		if got != c.want {
			t.Errorf("case %d body=%q: got %v, want %v", i, c.body, got, c.want)
		}
	}
}
