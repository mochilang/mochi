package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestPhase6Closures(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "swift", "fixtures", "phase06-closures")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")
		t.Run(name, func(t *testing.T) {
			runSwiftFixture(t, mochiPath, wantPath)
		})
	}
}
