package build

import (
	"path/filepath"
	"testing"
)

func TestPhase11Async(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase11-async"))
}
