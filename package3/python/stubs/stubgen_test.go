package stubs

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestFakeStubgenGenerate(t *testing.T) {
	cacheDir := t.TempDir()
	f := &FakeStubgen{CacheDir: cacheDir, Body: []byte("def f() -> int: ...\n")}
	src, err := f.Generate("mypkg", "mypkg", "")
	if err != nil {
		t.Fatal(err)
	}
	if src.Tier != TierStubgen {
		t.Errorf("Tier = %v, want TierStubgen", src.Tier)
	}
	if !src.Partial {
		t.Error("FakeStubgen must produce Partial = true")
	}
	if src.Package != "mypkg" {
		t.Errorf("Package = %q, want mypkg", src.Package)
	}
	wantPath := filepath.Join(cacheDir, "mypkg", "mypkg.pyi")
	body, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatal(err)
	}
	if string(body) != "def f() -> int: ...\n" {
		t.Errorf("body = %q", body)
	}
	if len(src.Files) != 1 || src.Files[0] != "mypkg.pyi" {
		t.Errorf("Files = %v", src.Files)
	}
}

func TestFakeStubgenEmptyCacheDir(t *testing.T) {
	f := &FakeStubgen{}
	if _, err := f.Generate("mypkg", "mypkg", ""); err == nil {
		t.Fatal("expected error on empty CacheDir")
	}
}

func TestFakeStubgenModuleAlias(t *testing.T) {
	cacheDir := t.TempDir()
	f := &FakeStubgen{CacheDir: cacheDir, Body: []byte("x: int\n")}
	// PEP 503 distribution name vs Python module name.
	src, err := f.Generate("Flask-SQLAlchemy", "flask_sqlalchemy", "")
	if err != nil {
		t.Fatal(err)
	}
	want := filepath.Join(cacheDir, "Flask-SQLAlchemy", "flask_sqlalchemy.pyi")
	if _, err := os.Stat(want); err != nil {
		t.Fatalf("expected %q to exist: %v", want, err)
	}
	if src.Files[0] != "flask_sqlalchemy.pyi" {
		t.Errorf("Files[0] = %q", src.Files[0])
	}
}

func TestNewExecStubgenDefaults(t *testing.T) {
	s := NewExecStubgen("/tmp/cache")
	if s.CacheDir != "/tmp/cache" {
		t.Errorf("CacheDir = %q", s.CacheDir)
	}
	if s.Timeout != 60*time.Second {
		t.Errorf("Timeout = %v, want 60s", s.Timeout)
	}
	if s.Python != "" {
		t.Errorf("Python = %q, want empty (PATH lookup)", s.Python)
	}
}

func TestExecStubgenEmptyCacheDir(t *testing.T) {
	s := &ExecStubgen{}
	if _, err := s.Generate("mypkg", "mypkg", ""); err == nil {
		t.Fatal("expected error on empty CacheDir")
	}
}

func TestExecStubgenMissingPython(t *testing.T) {
	// Set Python to a path that definitely doesn't exist and verify the error
	// path that runs the subprocess returns a wrapped error.
	cacheDir := t.TempDir()
	s := &ExecStubgen{
		Python:   "/nonexistent/python",
		CacheDir: cacheDir,
		Timeout:  2 * time.Second,
	}
	_, err := s.Generate("mypkg", "mypkg", "")
	if err == nil {
		t.Fatal("expected error from missing interpreter")
	}
	if !strings.Contains(err.Error(), "stubgen") {
		t.Errorf("error should mention stubgen: %v", err)
	}
}
