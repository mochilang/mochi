package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase3_3Sets runs the Phase 3.3 fixture suite.
// Gate: set literal, has, add, len, for-in iteration on BEAM.
func TestPhase3_3Sets(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase03_3")

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
