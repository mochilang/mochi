package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase13CrossOSCI is the MEP-45 Phase 13.3 gate. It verifies that the
// APE cross-OS CI workflow file exists and references the expected test IDs.
//
// Phase 13.3 adds .github/workflows/transpiler3-c-apex.yml, which:
//   - Builds one APE binary with cosmocc on ubuntu-latest.
//   - Uploads it as a GitHub Actions artifact.
//   - Runs it on ubuntu-latest, macos-latest, and windows-latest.
//
// The offline gate checks the workflow YAML structure rather than executing
// the full build, since cosmocc is not available in standard CI.
func TestPhase13CrossOSCI(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}

	t.Run("workflow_exists", func(t *testing.T) {
		root := repoRoot(t)
		wf := filepath.Join(root, ".github", "workflows", "transpiler3-c-apex.yml")
		if _, err := os.Stat(wf); err != nil {
			t.Fatalf("workflow file not found: %s: %v", wf, err)
		}
	})

	t.Run("workflow_references_apex", func(t *testing.T) {
		root := repoRoot(t)
		wf := filepath.Join(root, ".github", "workflows", "transpiler3-c-apex.yml")
		data, err := os.ReadFile(wf)
		if err != nil {
			t.Fatalf("read workflow: %v", err)
		}
		content := string(data)

		checks := []struct {
			name string
			want string
		}{
			{"has ubuntu runner", "ubuntu-latest"},
			{"has macos runner", "macos-latest"},
			{"has windows runner", "windows-latest"},
			{"references Phase13APE", "TestPhase13APE"},
			{"references cosmocc vendor", "TestPhase13CosmoVendor"},
			{"references MOCHI_COSMOCC_VENDOR_TEST", "MOCHI_COSMOCC_VENDOR_TEST"},
		}
		for _, c := range checks {
			if !strings.Contains(content, c.want) {
				t.Errorf("workflow missing %s (expected %q)", c.name, c.want)
			}
		}
	})
}
