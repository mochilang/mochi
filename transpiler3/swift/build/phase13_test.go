package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase13LLM is the gate test for MEP-49 Phase 13: generate expressions with cassette replay.
// Each fixture lives in its own subdirectory under tests/transpiler3/swift/fixtures/phase13-llm/.
// If a cassette/ subdirectory exists next to the .mochi file it is passed via
// MOCHI_LLM_CASSETTE_DIR so the runtime replays recorded responses without live API access.
func TestPhase13LLM(t *testing.T) {
	root := repoRoot(t)
	fixtureBase := filepath.Join(root, "tests", "transpiler3", "swift", "fixtures", "phase13-llm")
	entries, err := os.ReadDir(fixtureBase)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureBase, err)
	}

	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		fixtureDir := filepath.Join(fixtureBase, name)
		mochiPath := filepath.Join(fixtureDir, name+".mochi")
		wantPath := filepath.Join(fixtureDir, name+".out")

		if _, err := os.Stat(mochiPath); err != nil {
			continue
		}

		t.Run(name, func(t *testing.T) {
			var extraEnv []string
			cassetteDir := filepath.Join(fixtureDir, "cassette")
			if _, err := os.Stat(cassetteDir); err == nil {
				extraEnv = append(extraEnv, "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
			}
			runSwiftFixtureWithEnv(t, mochiPath, wantPath, extraEnv)
		})
	}
}
