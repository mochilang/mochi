package build

import (
	"path/filepath"
	"testing"
)

func TestPhase4Records(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase04-records"))
}
