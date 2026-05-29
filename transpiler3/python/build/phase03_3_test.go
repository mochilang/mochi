package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase33Sets iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase03-3-sets and runs
// runPythonFixture against the matching .out file.
func TestPhase33Sets(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase03-3-sets")
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
