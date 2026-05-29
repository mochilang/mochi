package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase13LLM iterates every fixture subdirectory under
// tests/transpiler3/python/fixtures/phase13-llm. Each subdirectory
// contains a single `<name>.mochi`, a matching `<name>.out`, and a
// `cassette/` directory whose <djb2hash>.txt files supply the LLM
// response. runPythonFixture detects the cassette dir alongside the
// .mochi and points MOCHI_LLM_CASSETTE_DIR at it; the generated
// module's `mochi_llm_generate(provider, model, prompt)` call then
// hashes the same way (DJB2 with NUL field separators) and reads the
// recorded response, byte-for-byte equal to the C target.
func TestPhase13LLM(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase13-llm")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		mochiPath := filepath.Join(fixtureDir, name, name+".mochi")
		wantPath := filepath.Join(fixtureDir, name, name+".out")

		if _, err := os.Stat(mochiPath); err != nil {
			// Skip directories that don't follow the layout (e.g.
			// scratch dirs). A real fixture is checked into the tree
			// with both files; an oversight surfaces as a t.Run miss
			// in CI rather than a panic here.
			t.Logf("skipping %s: %v", name, err)
			continue
		}

		t.Run(strings.TrimSuffix(filepath.Base(mochiPath), ".mochi"), func(t *testing.T) {
			runPythonFixture(t, mochiPath, wantPath)
		})
	}
}
