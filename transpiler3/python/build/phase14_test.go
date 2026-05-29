package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase14Fetch iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase14-fetch and runs
// runPythonFixture against the matching .out file. Phase 14.0 covers
// `fetch(url)` and `writeFile(path, content)` against the Python
// stdlib (urllib.request, open in binary mode). All fixtures use
// `file://` URLs so the test suite is hermetic; the same code path
// supports live `http(s)://` for production use without runtime swap.
func TestPhase14Fetch(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase14-fetch")
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
