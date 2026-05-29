package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase9Agents iterates every *.mochi file under
// tests/transpiler3/python/fixtures/phase09-agents and runs
// runPythonFixture against the matching .out file. The corpus covers
// synchronous agent intents (no spawn) and bounded FIFO channels
// (collections.deque). Async / spawn / cross-task channels are in
// Phase 10.
func TestPhase9Agents(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase09-agents")
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
