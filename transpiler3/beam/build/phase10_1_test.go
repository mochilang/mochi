package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase10_1Channels runs the Phase 10.1 channel fixture suite.
func TestPhase10_1Channels(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase10_1")

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
		t.Fatal("no fixtures found in phase10_1/")
	}
}
