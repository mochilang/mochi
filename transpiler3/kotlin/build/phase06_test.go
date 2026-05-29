package build

import (
	"path/filepath"
	"testing"
)

func TestPhase6Closures(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase06-closures"))
}
