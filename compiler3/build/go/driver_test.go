package gobuild

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	gogen "mochi/compiler3/emit/go"
	"mochi/compiler3/ir"
)

// TestBuildExecutableWritesGenFile asserts the driver writes a gen.go
// containing the emitter's output for the FibIter fixture, with the
// requested PkgName.
func TestBuildExecutableWritesGenFile(t *testing.T) {
	dir := t.TempDir()
	p := &gogen.Program{
		PkgName: "demo",
		Funcs:   []*ir.Function{ir.FixtureFibIter()},
	}
	r, err := Build(p, Options{Mode: ModeExecutable, OutDir: dir, PkgName: "demo"})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if len(r.Files) != 1 || r.EntryPoint == "" {
		t.Fatalf("expected one file + EntryPoint, got %+v", r)
	}
	src, err := os.ReadFile(r.EntryPoint)
	if err != nil {
		t.Fatalf("read gen.go: %v", err)
	}
	if !strings.Contains(string(src), "package demo") {
		t.Errorf("expected package demo in gen.go:\n%s", src)
	}
	if !strings.Contains(string(src), "func fib_iter") {
		t.Errorf("expected fib_iter func in gen.go:\n%s", src)
	}
}

// TestBuildLibraryRequiresModulePath asserts ModeLibrary without a
// ModulePath returns an error rather than silently emitting a broken
// go.mod.
func TestBuildLibraryRequiresModulePath(t *testing.T) {
	dir := t.TempDir()
	p := &gogen.Program{Funcs: []*ir.Function{ir.FixtureFibIter()}}
	_, err := Build(p, Options{Mode: ModeLibrary, OutDir: dir})
	if err == nil {
		t.Fatal("Build with ModeLibrary should require ModulePath")
	}
}

// TestBuildLibraryEmitsPkgAndMod asserts ModeLibrary produces pkg.go +
// go.mod, with the module path baked into go.mod.
func TestBuildLibraryEmitsPkgAndMod(t *testing.T) {
	dir := t.TempDir()
	p := &gogen.Program{Funcs: []*ir.Function{ir.FixtureFibIter()}}
	r, err := Build(p, Options{
		Mode:       ModeLibrary,
		OutDir:     dir,
		ModulePath: "example.com/mypkg",
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	hasGoMod, hasPkgGo := false, false
	for _, f := range r.Files {
		switch filepath.Base(f) {
		case "go.mod":
			hasGoMod = true
		case "mypkg.go":
			hasPkgGo = true
		}
	}
	if !hasGoMod || !hasPkgGo {
		t.Errorf("missing files: go.mod=%v mypkg.go=%v files=%v", hasGoMod, hasPkgGo, r.Files)
	}
	modSrc, err := os.ReadFile(filepath.Join(dir, "go.mod"))
	if err != nil {
		t.Fatalf("read go.mod: %v", err)
	}
	if !strings.Contains(string(modSrc), "module example.com/mypkg") {
		t.Errorf("go.mod missing module directive:\n%s", modSrc)
	}
}

// TestCleanupRemovesFilesWhenKeepEmitFalse asserts the Cleanup helper
// honors KeepEmit=false by removing the produced files. When KeepEmit
// is true, the files must remain.
func TestCleanupRemovesFilesWhenKeepEmitFalse(t *testing.T) {
	dir := t.TempDir()
	p := &gogen.Program{
		PkgName: "demo",
		Funcs:   []*ir.Function{ir.FixtureFibIter()},
	}
	opts := Options{Mode: ModeExecutable, OutDir: dir, PkgName: "demo"}
	r, err := Build(p, opts)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if _, err := os.Stat(r.EntryPoint); err != nil {
		t.Fatalf("stat before cleanup: %v", err)
	}
	if err := Cleanup(r, opts); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(r.EntryPoint); !os.IsNotExist(err) {
		t.Errorf("Cleanup with KeepEmit=false should remove gen file, got err=%v", err)
	}
}

// TestCleanupKeepsFilesWhenKeepEmitTrue asserts the Cleanup helper is a
// no-op when KeepEmit=true.
func TestCleanupKeepsFilesWhenKeepEmitTrue(t *testing.T) {
	dir := t.TempDir()
	p := &gogen.Program{
		PkgName: "demo",
		Funcs:   []*ir.Function{ir.FixtureFibIter()},
	}
	opts := Options{Mode: ModeExecutable, OutDir: dir, PkgName: "demo", KeepEmit: true}
	r, err := Build(p, opts)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if err := Cleanup(r, opts); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(r.EntryPoint); err != nil {
		t.Errorf("KeepEmit=true should preserve gen file, got err=%v", err)
	}
}

// TestBuildRejectsMissingOutDir asserts the driver refuses to run
// without an OutDir rather than scattering files in the cwd.
func TestBuildRejectsMissingOutDir(t *testing.T) {
	p := &gogen.Program{Funcs: []*ir.Function{ir.FixtureFibIter()}}
	_, err := Build(p, Options{Mode: ModeExecutable})
	if err == nil {
		t.Fatal("Build without OutDir should fail")
	}
}
