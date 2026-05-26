package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase13LLMStructured runs the Phase 13.1 structured-output fixture suite.
// Gate: generate expressions with a schema field compile and cassette playback
// produces byte-equal output; schema is appended to the prompt as a JSON schema hint.
func TestPhase13LLMStructured(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase13_llm_structured")

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
		t.Fatal("no fixtures found in phase13_llm_structured/")
	}
}
