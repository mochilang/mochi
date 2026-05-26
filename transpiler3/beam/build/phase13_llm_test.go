package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase13LLM runs the Phase 13.0 LLM fixture suite.
// Gate: generate expressions compile and cassette playback produces
// byte-equal output vs vm3/C oracle; 5 fixtures (TestPhase13LLM green).
func TestPhase13LLM(t *testing.T) {
	root := repoRoot(t)
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase13_llm")

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
