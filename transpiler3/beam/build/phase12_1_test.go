package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase12_1ExternErlang runs the Phase 12.1 extern Erlang FFI fixture suite.
// Phase 12.1 gates `extern fun module.function(args): T` declarations that
// call Erlang stdlib functions directly via module:function/arity calls on BEAM.
func TestPhase12_1ExternErlang(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase12_1")

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
		t.Fatal("no fixtures found in phase12_1/")
	}
}
