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
// (collections.deque). Sub-phase 9.1 (spawn + async cast/call over
// asyncio.Queue) is deferred to Phase 10's async colour pass; 9.2
// (TaskGroup supervision), 9.3 (ExceptionGroup unwrap), and 9.4
// (named-agent Registry) are deferred to Phase 11's error model and
// runtime registry work.
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
