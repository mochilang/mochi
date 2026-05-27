package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase8Datalog runs the Phase 8.0 Datalog fixture suite.
// Gate: compile-time semi-naive evaluation produces byte-equal output vs
// vm3/C oracle; 3 fixtures (TestPhase8Datalog green).
func TestPhase8Datalog(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase08_datalog")

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
