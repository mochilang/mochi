package build

import (
	"path/filepath"
	"testing"
)

func TestPhase12FFI(t *testing.T) {
	runFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase12-ffi"))
}
