package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase9_1SpawnAgent runs the Phase 9.1 fixture suite.
// Phase 9.1 gates `spawn AgentType()` which creates a message-loop process
// (via mochi_agent_server) and returns an opaque PID. Intent calls on the PID
// use mochi_agent_server:call for value-returning intents and
// mochi_agent_server:cast for unit intents.
func TestPhase9_1SpawnAgent(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase09_1")

	entries, err := os.ReadDir(fixturesDir)
	if err != nil {
		t.Fatalf("read fixtures dir %s: %v", fixturesDir, err)
	}

	ran := 0
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		dir := filepath.Join(fixturesDir, name)

		mochiPath := filepath.Join(dir, name+".mochi")
		expectPath := filepath.Join(dir, "expect.txt")

		if _, err := os.Stat(mochiPath); err != nil {
			t.Logf("skip %s: no .mochi file", name)
			continue
		}
		if _, err := os.Stat(expectPath); err != nil {
			t.Logf("skip %s: no expect.txt", name)
			continue
		}

		t.Run(name, func(t *testing.T) {
			runBeamFixture(t, mochiPath, expectPath)
		})
		ran++
	}

	if ran == 0 {
		t.Fatal("no fixtures found in phase09_1/")
	}
}
