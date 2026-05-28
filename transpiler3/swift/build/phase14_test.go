package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase14Fetch is the gate test for MEP-49 Phase 14: HTTP fetch via mochiHttpGet.
// Fixtures use file:// URLs so no real HTTP server is needed.
func TestPhase14Fetch(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "swift", "fixtures", "phase14-fetch")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runSwiftFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}
