package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase9_3OnClose runs the Phase 9.3 fixture suite.
// Phase 9.3 gates `on close { ... }` blocks inside agent declarations.
// The block lowers to a terminate/1 helper function and is passed to
// mochi_agent_server:start/3 as the optional terminate callback.
// Normal intent calls must still work when on_close is present.
func TestPhase9_3OnClose(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase09_3")

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
		t.Fatal("no fixtures found in phase09_3/")
	}
}
