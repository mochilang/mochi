package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestDialyzer runs Dialyzer on the Mochi BEAM runtime source files
// (.erl in transpiler3/beam/runtime/src/) and asserts zero warnings.
// Phase 17.2 gate: dialyzer -Werror passes on all runtime modules.
//
// The test is skipped if dialyzer is not on PATH (e.g., CI environments
// that don't have OTP tools in PATH). The blocking CI matrix always has
// dialyzer available via erlef/setup-beam.
func TestDialyzer(t *testing.T) {
	dialyzer, err := exec.LookPath("dialyzer")
	if err != nil {
		t.Skip("dialyzer not on PATH; skipping")
	}

	root := repoRoot(t)
	runtimeSrc := filepath.Join(root, "transpiler3", "beam", "runtime", "src")
	if _, err := os.Stat(runtimeSrc); err != nil {
		t.Skipf("runtime src not found: %v", err)
	}

	// Build a PLT covering core OTP apps so dialyzer can resolve
	// types used by the runtime source files. Exit code 2 means
	// "PLT built with warnings" (OTP internal cross-app references)
	// which is harmless and expected for a subset build.
	pltPath := filepath.Join(t.TempDir(), "mochi_runtime.plt")
	buildPlt := exec.Command(dialyzer, "--build_plt",
		"--apps", "erts", "kernel", "stdlib", "inets", "public_key", "ssl",
		"--output_plt", pltPath)
	buildOut, buildErr := buildPlt.CombinedOutput()
	if buildErr != nil && buildPlt.ProcessState.ExitCode() == 1 {
		t.Fatalf("dialyzer --build_plt failed: %v\n%s", buildErr, buildOut)
	}

	// Collect all .erl source files in the runtime directory.
	entries, err := os.ReadDir(runtimeSrc)
	if err != nil {
		t.Fatalf("readdir %s: %v", runtimeSrc, err)
	}
	var erlFiles []string
	for _, e := range entries {
		if filepath.Ext(e.Name()) == ".erl" {
			erlFiles = append(erlFiles, filepath.Join(runtimeSrc, e.Name()))
		}
	}
	if len(erlFiles) == 0 {
		t.Fatal("no .erl files found in runtime/src/")
	}

	// Run dialyzer on all runtime .erl files.
	args := append([]string{"--plt", pltPath}, erlFiles...)
	cmd := exec.Command(dialyzer, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Errorf("dialyzer found warnings or errors in runtime sources:\n%s", out)
	}
}
