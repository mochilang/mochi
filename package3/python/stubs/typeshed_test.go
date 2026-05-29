package stubs

import (
	"os"
	"path/filepath"
	"testing"
)

func TestNewTypeshedRejectsEmptyRoot(t *testing.T) {
	if _, err := NewTypeshed("", ""); err == nil {
		t.Fatal("expected error for empty root")
	}
}

func TestNewTypeshedRejectsMissingRoot(t *testing.T) {
	if _, err := NewTypeshed(filepath.Join(t.TempDir(), "nope"), ""); err == nil {
		t.Fatal("expected error for missing root")
	}
}

func TestNewTypeshedRejectsRegularFile(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, "afile")
	if err := os.WriteFile(p, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := NewTypeshed(p, ""); err == nil {
		t.Fatal("expected error: root is not a directory")
	}
}

func TestNewTypeshedRejectsNonTypeshedDir(t *testing.T) {
	dir := t.TempDir() // empty
	if _, err := NewTypeshed(dir, ""); err == nil {
		t.Fatal("expected error: missing stdlib/ or stubs/")
	}
}

func TestNewTypeshedAcceptsStdlibOnly(t *testing.T) {
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "stdlib"), 0o755); err != nil {
		t.Fatal(err)
	}
	ts, err := NewTypeshed(dir, "abc123")
	if err != nil {
		t.Fatal(err)
	}
	if ts.Commit != "abc123" {
		t.Errorf("Commit = %q, want abc123", ts.Commit)
	}
}

func TestNewTypeshedAcceptsStubsOnly(t *testing.T) {
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "stubs"), 0o755); err != nil {
		t.Fatal(err)
	}
	if _, err := NewTypeshed(dir, ""); err != nil {
		t.Fatal(err)
	}
}

func TestLookupStdlibDir(t *testing.T) {
	dir := t.TempDir()
	pkgDir := filepath.Join(dir, "stdlib", "json")
	if err := os.MkdirAll(pkgDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkgDir, "__init__.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	ts, _ := NewTypeshed(dir, "")
	src, ok := ts.Lookup("json")
	if !ok {
		t.Fatal("expected hit")
	}
	if src.Tier != TierTypeshed {
		t.Errorf("Tier = %v, want TierTypeshed", src.Tier)
	}
	if src.RootDir != pkgDir {
		t.Errorf("RootDir = %q, want %q", src.RootDir, pkgDir)
	}
}

func TestLookupStdlibFile(t *testing.T) {
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "stdlib"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "stdlib", "os.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	ts, _ := NewTypeshed(dir, "")
	src, ok := ts.Lookup("os")
	if !ok {
		t.Fatal("expected hit")
	}
	if src.Tier != TierTypeshed {
		t.Errorf("Tier = %v, want TierTypeshed", src.Tier)
	}
	if len(src.Files) != 1 || src.Files[0] != "os.pyi" {
		t.Errorf("Files = %v, want [os.pyi]", src.Files)
	}
}

func TestLookupThirdPartyNested(t *testing.T) {
	dir := t.TempDir()
	tsPkg := filepath.Join(dir, "stubs", "requests", "requests")
	if err := os.MkdirAll(tsPkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tsPkg, "__init__.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(dir, "stdlib"), 0o755); err != nil {
		t.Fatal(err)
	}
	ts, _ := NewTypeshed(dir, "")
	src, ok := ts.Lookup("requests")
	if !ok {
		t.Fatal("expected hit")
	}
	if src.RootDir != tsPkg {
		t.Errorf("RootDir = %q, want %q", src.RootDir, tsPkg)
	}
}

func TestLookupThirdPartyFlat(t *testing.T) {
	dir := t.TempDir()
	tsPkg := filepath.Join(dir, "stubs", "toml")
	if err := os.MkdirAll(tsPkg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tsPkg, "__init__.pyi"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	ts, _ := NewTypeshed(dir, "")
	src, ok := ts.Lookup("toml")
	if !ok {
		t.Fatal("expected hit")
	}
	if src.RootDir != tsPkg {
		t.Errorf("RootDir = %q, want %q", src.RootDir, tsPkg)
	}
}

func TestLookupMiss(t *testing.T) {
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "stdlib"), 0o755); err != nil {
		t.Fatal(err)
	}
	ts, _ := NewTypeshed(dir, "")
	if _, ok := ts.Lookup("nope"); ok {
		t.Fatal("expected miss")
	}
}

func TestModuleFromName(t *testing.T) {
	cases := []struct {
		name, want string
	}{
		{"requests", "requests"},
		{"Flask", "flask"},
		{"Flask-SQLAlchemy", "flask_sqlalchemy"},
		{"PyYAML", "pyyaml"},
		{"numpy", "numpy"},
		{"", ""},
	}
	for _, c := range cases {
		if got := moduleFromName(c.name); got != c.want {
			t.Errorf("moduleFromName(%q) = %q, want %q", c.name, got, c.want)
		}
	}
}

func TestDirExists(t *testing.T) {
	dir := t.TempDir()
	if !dirExists(dir) {
		t.Error("temp dir should exist")
	}
	if dirExists(filepath.Join(dir, "nope")) {
		t.Error("missing dir should not exist")
	}
	p := filepath.Join(dir, "f.txt")
	if err := os.WriteFile(p, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if dirExists(p) {
		t.Error("regular file should not register as dir")
	}
}
