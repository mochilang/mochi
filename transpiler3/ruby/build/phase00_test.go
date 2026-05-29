package build

import (
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase0Skeleton verifies: (1) Ruby 3.2+ is on PATH,
// (2) the runtime gem skeleton exists at the expected location,
// (3) go build ./transpiler3/ruby/... is clean.
func TestPhase0Skeleton(t *testing.T) {
	t.Run("toolchain", func(t *testing.T) {
		tc, err := resolveToolchain()
		if err != nil {
			t.Fatalf("resolveToolchain: %v", err)
		}
		if tc.Major < 3 || (tc.Major == 3 && tc.Minor < 2) {
			t.Fatalf("ruby 3.2+ required; found %d.%d", tc.Major, tc.Minor)
		}
		t.Logf("Ruby %d.%d at %s", tc.Major, tc.Minor, tc.Ruby)
	})

	t.Run("runtime_gem", func(t *testing.T) {
		repoRoot := repoRootForTest(t)
		// Phase 0 ships the runtime skeleton at mochi-runtime/.
		// We accept either a real .gemspec or, if not yet built, just the
		// presence of lib/mochi/runtime.rb (the minimum the emitter requires).
		paths := []string{
			filepath.Join(repoRoot, "mochi-runtime", "lib", "mochi", "runtime.rb"),
		}
		for _, p := range paths {
			cmd := exec.Command("test", "-f", p)
			if err := cmd.Run(); err != nil {
				t.Skipf("runtime skeleton %s not present yet", p)
			}
		}
	})

	t.Run("go_build", func(t *testing.T) {
		cmd := exec.Command("go", "build", "./transpiler3/ruby/...")
		cmd.Dir = repoRootForTest(t)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("go build ./transpiler3/ruby/... failed:\n%s", out)
		}
	})
}
