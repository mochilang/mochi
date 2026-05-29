package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase15GoModule gates the MEP-54 Phase 15 target
// `go-module`. When target == TargetGoModule, Driver.Build
// must skip `go build` and emit a publish-ready Go module to
// the out directory: at minimum go.mod + main.go.
//
// The test then runs `go build .` against the copied module
// to prove the emitted artifact is buildable on its own; the
// produced binary must execute and match expect.txt.
func TestPhase15GoModule(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")
	want, err := os.ReadFile(filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	outDir := filepath.Join(t.TempDir(), "module-out")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outDir, string(TargetGoModule), ""); err != nil {
		t.Fatalf("Driver.Build (go-module): %v", err)
	}

	for _, name := range []string{"main.go", "go.mod"} {
		if _, err := os.Stat(filepath.Join(outDir, name)); err != nil {
			t.Fatalf("module out missing %s: %v", name, err)
		}
	}

	// Build the copied module from outDir and exec the binary.
	binPath := filepath.Join(t.TempDir(), "hello-from-module")
	cmd := exec.Command("go", "build", "-trimpath", "-buildvcs=false", "-o", binPath, ".")
	cmd.Dir = outDir
	var bstderr bytes.Buffer
	cmd.Stderr = &bstderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("go build copied module: %v\n%s", err, bstderr.String())
	}

	run := exec.Command(binPath)
	var stdout bytes.Buffer
	run.Stdout = &stdout
	run.Stderr = os.Stderr
	if err := run.Run(); err != nil {
		t.Fatalf("run copied-module binary: %v", err)
	}
	if got := stdout.String(); got != string(want) {
		t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
	}
}

// TestPhase15GoModuleContent verifies the go.mod copied into
// the module out directory matches the canonical template
// (module mochi_userprog + go 1.26.0 + toolchain go1.26.3)
// and that main.go declares package main with a main func.
func TestPhase15GoModuleContent(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")
	outDir := filepath.Join(t.TempDir(), "module-out")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outDir, string(TargetGoModule), ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}

	modBytes, err := os.ReadFile(filepath.Join(outDir, "go.mod"))
	if err != nil {
		t.Fatalf("read go.mod: %v", err)
	}
	for _, marker := range []string{"module mochi_userprog", "go 1.26.0", "toolchain go1.26.3"} {
		if !strings.Contains(string(modBytes), marker) {
			t.Fatalf("go.mod missing %q:\n%s", marker, string(modBytes))
		}
	}

	mainBytes, err := os.ReadFile(filepath.Join(outDir, "main.go"))
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	for _, marker := range []string{"package main", "func main()"} {
		if !strings.Contains(string(mainBytes), marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, string(mainBytes))
		}
	}
}

// TestPhase15GoModuleCustomModulePath verifies that
// Driver.ModulePath is honored when emitting a go-module
// artifact (publish flow needs to control the module name).
func TestPhase15GoModuleCustomModulePath(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")
	outDir := filepath.Join(t.TempDir(), "module-out")
	d := &Driver{
		CacheDir:   t.TempDir(),
		ModulePath: "example.com/userprog",
	}
	if err := d.Build(src, outDir, string(TargetGoModule), ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	modBytes, err := os.ReadFile(filepath.Join(outDir, "go.mod"))
	if err != nil {
		t.Fatalf("read go.mod: %v", err)
	}
	if !strings.Contains(string(modBytes), "module example.com/userprog") {
		t.Fatalf("custom ModulePath not honored:\n%s", string(modBytes))
	}
}
