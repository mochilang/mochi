package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase1Hello iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase01-hello and runs runPythonFixture
// against the matching .out file.
func TestPhase1Hello(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase01-hello")
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
