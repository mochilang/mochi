package build

import (
	"path/filepath"
	"testing"
)

func TestPhase10Streams(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase10-streams"))
}
