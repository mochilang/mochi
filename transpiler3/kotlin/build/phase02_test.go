package build

import (
	"path/filepath"
	"testing"
)

func TestPhase2Scalars(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase02-scalars"))
}
