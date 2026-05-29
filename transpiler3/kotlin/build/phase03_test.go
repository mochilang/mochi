package build

import (
	"path/filepath"
	"testing"
)

func TestPhase3Lists(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase03-lists"))
}

func TestPhase3Maps(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase03-maps"))
}

func TestPhase3Sets(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase03-sets"))
}
