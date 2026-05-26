package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase9_4SupervisedAgents runs the Phase 9.4 fixture suite.
// Phase 9.4 gates supervised crash + restart via OTP's dynamic_supervisor.
// mochi_agent_sup.erl provides a simple_one_for_one supervisor with transient
// restart policy; mochi_sup.erl starts mochi_agent_sup as a permanent child.
// In escript mode the supervisor is not started automatically, but the module
// compiles with the runtime and will be active in full OTP release mode.
// The fixture verifies the runtime compiles with the supervisor present
// and that normal agent spawning still works.
func TestPhase9_4SupervisedAgents(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase09_4")

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
		t.Fatal("no fixtures found in phase09_4/")
	}
}
