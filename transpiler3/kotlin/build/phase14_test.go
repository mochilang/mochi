package build

import (
	"os"
	"path/filepath"
	"testing"
)

func TestPhase14Fetch(t *testing.T) {
	if os.Getenv("MOCHI_TEST_HTTP_BASE") == "" {
		t.Skip("MOCHI_TEST_HTTP_BASE not set; skipping HTTP fetch tests")
	}
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase14-fetch"))
}
