package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase13APE is the MEP-45 Phase 13.0 gate. It compiles the
// primitives/add_ints fixture as an Actually Portable Executable via
// cosmocc and runs the produced binary on the current OS.
//
// cosmocc produces a single binary that runs natively on Linux, macOS,
// Windows, FreeBSD, NetBSD, and OpenBSD without separate toolchain
// installs. The driver skips -target, -ffile-prefix-map, -Wl,-no_uuid,
// -static, and sanitiser flags for Apex builds (cosmocc handles all of
// these internally via its own cosmopolitan libc toolchain).
//
// Gate skip conditions (test returns without failure):
//   - MOCHI_COSMOCC_PATH is unset AND cosmocc is not on PATH.
//
// To run locally:
//
//	MOCHI_COSMOCC_PATH=/path/to/cosmocc go test ./transpiler3/c/build/... -run TestPhase13APE -v
func TestPhase13APE(t *testing.T) {
	cosmoccPath := os.Getenv("MOCHI_COSMOCC_PATH")
	if cosmoccPath == "" {
		var err error
		cosmoccPath, err = exec.LookPath("cosmocc")
		if err != nil {
			t.Skip("cosmocc not found: set MOCHI_COSMOCC_PATH or install cosmocc on PATH; skipping Phase 13.0 APE gate")
		}
	}

	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "primitives", "add_ints", "add_ints.mochi")
	want := []byte("3\n")

	outBin := filepath.Join(t.TempDir(), "add_ints_ape")
	d := &Driver{
		CacheDir: t.TempDir(),
		NoCache:  true,
		Apex:     true,
	}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build(Apex=true): %v", err)
	}

	cmd := exec.Command(outBin)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run APE binary: %v", err)
	}
	if !bytes.Equal(stdout.Bytes(), want) {
		t.Fatalf("output mismatch: want %q got %q", want, stdout.Bytes())
	}
}
