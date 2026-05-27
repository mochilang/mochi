package build

import (
	"os/exec"
	"testing"
)

func TestPhase0Skeleton(t *testing.T) {
	t.Run("toolchain", func(t *testing.T) {
		tc, err := resolveToolchain()
		if err != nil {
			t.Skipf("dotnet SDK not found: %v", err)
		}
		if tc.Major < 8 {
			t.Fatalf("dotnet SDK 8+ required; found %d", tc.Major)
		}
		t.Logf("dotnet SDK %d.%d.%d at %s", tc.Major, tc.Minor, tc.Patch, tc.Dotnet)
	})

	t.Run("go_build", func(t *testing.T) {
		repoRoot := repoRootForBuild(t)
		cmd := exec.Command("go", "build", "./transpiler3/dotnet/...")
		cmd.Dir = repoRoot
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("go build ./transpiler3/dotnet/... failed:\n%s", out)
		}
	})
}
