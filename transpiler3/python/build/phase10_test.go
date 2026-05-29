package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase10Streams iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase10-streams and runs
// runPythonFixture against the matching .out file. Phase 10.0
// covers the synchronous broadcast surface: make_stream, subscribe,
// emit, recv_sub. Each subscriber holds an independent read cursor
// over a shared append-only buffer (matches the C-fixture corpus
// at tests/transpiler3/c/fixtures/stream/).
//
// Async / cross-task broadcast / bounded backpressure is Phase 11+.
func TestPhase10Streams(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase10-streams")
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
