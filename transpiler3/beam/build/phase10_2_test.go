package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase10_2SubscribeLimit runs the Phase 10.2 fixture suite.
// Phase 10.2 gates subscriber backpressure: `subscribe_limit(stream, N)`
// drops incoming messages when the subscriber's buffer already holds N items.
// The underlying runtime is mochi_stream:subscribe_limit/2 which spawns a
// sub_loop that checks the buffer length before accepting each event.
func TestPhase10_2SubscribeLimit(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase10_2")

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
		t.Fatal("no fixtures found in phase10_2/")
	}
}
