package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase13LLM exercises `generate <provider> { ... }` expressions with
// cassette replay. Each fixture lives in its own subdirectory; the cassette
// dir (when present) is passed via MOCHI_LLM_CASSETTE_DIR so
// mochi_runtime::llm::call replays the recorded response.
func TestPhase13LLM(t *testing.T) {
	fixtureBase := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase13-llm")
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
			runRustFixtureWithEnv(t, mochiPath, wantPath, extraEnv)
		})
	}
}
