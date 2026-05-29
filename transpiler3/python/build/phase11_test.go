package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase11ErrorModel iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase11-error-model and runs
// runPythonFixture against the matching .out file. Phase 11.0 covers
// the synchronous error surface: user-raised `panic(code, msg)`,
// recoverable `try { ... } catch e { ... }` blocks binding the integer
// code, and the built-in fault rewrite (ZeroDivisionError -> 5,
// IndexError -> 4). Async + MochiResult / asyncio.Future are deferred
// to Phase 11.1 since no v1 fixtures exercise them.
func TestPhase11ErrorModel(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase11-error-model")
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
