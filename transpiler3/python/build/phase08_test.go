package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase8Datalog iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase08-datalog and runs
// runPythonFixture against the matching .out file. Datalog programs
// are evaluated at compile time by the Python lowerer and emitted as a
// static list[str] literal, mirroring the BEAM backend's strategy.
func TestPhase8Datalog(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase08-datalog")
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
