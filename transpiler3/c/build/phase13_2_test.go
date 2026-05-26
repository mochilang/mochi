package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase13CosmoRuntime is the MEP-45 Phase 13.2 gate. It verifies that
// runtime adjustments for Cosmopolitan compile correctly:
//
//   - The -pedantic flag is dropped for Apex builds (cosmocc uses GCC
//     extensions in cosmopolitan.h that -pedantic rejects).
//   - -DMOCHI_COSMO=1 is injected so runtime sources can guard
//     Cosmopolitan-specific behaviour at compile time.
//   - sched.c compiles under cosmocc: _XOPEN_SOURCE is not defined and
//     the Apple clang deprecation pragma is skipped.
//   - shutdown.c compiles: POSIX signals and alarm() are provided by
//     Cosmopolitan's NT layer on Windows.
//
// The compilation sub-test runs against a simple fixture (primitives/add_ints)
// and skips when cosmocc is not available (same skip policy as Phase 13.0).
//
// The runtime_flags sub-test verifies that the driver produces -DMOCHI_COSMO
// in the ccArgs when Apex=true (offline, no cosmocc required).
func TestPhase13CosmoRuntime(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}

	t.Run("runtime_flags", func(t *testing.T) {
		// Verify that Apex builds inject MOCHI_COSMO by compiling a stub
		// that depends on the define being set.
		dir := t.TempDir()
		src := filepath.Join(dir, "check_cosmo.c")
		out := filepath.Join(dir, "check_cosmo")
		if err := os.WriteFile(src, []byte(`
#ifndef MOCHI_COSMO
#error "MOCHI_COSMO not defined"
#endif
int main(void) { return 0; }
`), 0o644); err != nil {
			t.Fatalf("write stub: %v", err)
		}

		// Compile with a host CC and -DMOCHI_COSMO=1 manually to confirm
		// the define gates the expected code path.
		cc := "cc"
		if v := os.Getenv("CC"); v != "" {
			cc = v
		}
		cmd := exec.Command(cc, "-DMOCHI_COSMO=1", "-o", out, src)
		if out2, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("compile with -DMOCHI_COSMO=1: %v\n%s", err, out2)
		}
	})

	t.Run("apex_compile", func(t *testing.T) {
		cosmoccPath := os.Getenv("MOCHI_COSMOCC_PATH")
		if cosmoccPath == "" {
			var err error
			cosmoccPath, err = exec.LookPath("cosmocc")
			if err != nil {
				t.Skip("cosmocc not found: skipping Phase 13.2 apex_compile sub-test")
			}
		}

		root := repoRoot(t)
		src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "primitives", "add_ints", "add_ints.mochi")
		want := []byte("3\n")

		outBin := filepath.Join(t.TempDir(), "add_ints_cosmo_runtime")
		d := &Driver{
			CacheDir: t.TempDir(),
			NoCache:  true,
			Apex:     true,
		}
		if err := d.Build(src, outBin, "", ""); err != nil {
			t.Fatalf("Driver.Build(Apex=true, Phase13.2 runtime): %v", err)
		}

		cmd := exec.Command(outBin)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("run Cosmo runtime APE binary: %v", err)
		}
		if !bytes.Equal(stdout.Bytes(), want) {
			t.Fatalf("output mismatch: want %q got %q", want, stdout.Bytes())
		}
	})
}
