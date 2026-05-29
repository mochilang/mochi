package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase12FFI iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase12-ffi and runs runPythonFixture
// against the matching .out file. Phase 12.0 covers the Python FFI
// surface: `extern python fun X(...)` lowers to a `from <pkg>_externs
// import X` at the top of the generated module; the build copies a
// sidecar `<name>_externs.py` from next to the .mochi into the
// generated src/ tree. Go FFI, JS FFI, Java FFI, and C extern decls
// reject at lower time with an explicit error (out of scope for the
// Python target).
func TestPhase12FFI(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase12-ffi")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")

		t.Run(name, func(t *testing.T) {
			runPythonFixture(t, mochiPath, wantPath)
		})
	}
}
