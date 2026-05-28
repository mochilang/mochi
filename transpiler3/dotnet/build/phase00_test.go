package build

import (
	"os"
	"os/exec"
	"path/filepath"
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

	t.Run("runtime_nupkg", func(t *testing.T) {
		tc, err := resolveToolchain()
		if err != nil {
			t.Skipf("dotnet SDK not found: %v", err)
		}
		runtimeCsproj := runtimeCsprojPath()
		if runtimeCsproj == "" {
			t.Fatal("Mochi.Runtime.csproj not found")
		}
		outDir := t.TempDir()
		cmd := exec.Command(tc.Dotnet, "pack", runtimeCsproj, "-c", "Release", "-o", outDir, "--nologo", "-v", "minimal")
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("dotnet pack Mochi.Runtime.csproj failed:\n%s", out)
		}
		entries, err := filepath.Glob(filepath.Join(outDir, "Mochi.Runtime.*.nupkg"))
		if err != nil || len(entries) == 0 {
			// Check also for any .nupkg
			all, _ := os.ReadDir(outDir)
			t.Fatalf("expected Mochi.Runtime.*.nupkg in %s; got %v", outDir, all)
		}
		t.Logf("nupkg: %s", entries[0])
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
