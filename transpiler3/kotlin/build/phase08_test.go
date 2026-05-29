package build

import (
	"path/filepath"
	"testing"
)

func TestPhase8Datalog(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase08-datalog"))
}
