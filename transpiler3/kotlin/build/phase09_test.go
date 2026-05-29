package build

import (
	"path/filepath"
	"testing"
)

func TestPhase9Agents(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase09-agents"))
}
