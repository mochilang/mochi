package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase3_5ListOfRecord runs the Phase 3.5 fixture suite.
// Gate: list<Record> (literal, index, for-each, append, func arg, filter) on BEAM.
func TestPhase3_5ListOfRecord(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase03_5")

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
		mochi := filepath.Join(dir, name+".mochi")
		expect := filepath.Join(dir, "expect.txt")
		t.Run(name, func(t *testing.T) {
			runBeamFixture(t, mochi, expect)
		})
		ran++
	}
	if ran == 0 {
		t.Fatal("no fixtures found")
	}
}
