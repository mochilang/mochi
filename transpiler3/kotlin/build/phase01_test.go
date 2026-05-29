package build

import (
	"path/filepath"
	"testing"
)

func TestPhase1Hello(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase01-hello"))
}
