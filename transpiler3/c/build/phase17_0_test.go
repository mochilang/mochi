package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase17Repro is the MEP-45 Phase 17.0 + 17.1 gate. It builds
// the same fixture twice (different tempdirs, same basename so the
// Mach-O code-signature identifier is stable) with a fixed
// SOURCE_DATE_EPOCH and asserts byte-identical SHA-256 hashes.
//
// This validates:
//   - Phase 17.0: SOURCE_DATE_EPOCH is honoured; no __DATE__/__TIME__
//     macros are expanded in the emitted or runtime C.
//   - Phase 17.1: -ffile-prefix-map and -fdebug-prefix-map strip the
//     random tempdir path from any debug info; -Wl,-no_uuid (macOS)
//     suppresses the random LC_UUID load command.
//
// The fixture chosen is `primitives/add_ints`: small compile, no I/O,
// deterministic output, exercises the full pipeline.
//
// Note: SOURCE_DATE_EPOCH is passed via os.Environ() inheritance. The
// driver invokes cc without overriding cmd.Env, so the variable flows
// through naturally. Tests that need a specific epoch set it in the
// process environment via t.Setenv.
func TestPhase17Repro(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("reproducibility gate skipped on Windows; Phase 11 wires Windows CI")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "primitives", "add_ints", "add_ints.mochi")

	const fixedEpoch = "1748000000"
	t.Setenv("SOURCE_DATE_EPOCH", fixedEpoch)

	// Build twice into different tempdirs but with the same output
	// basename ("add_ints"). On macOS the Mach-O code-signature
	// identifier is derived from the basename, so using the same name
	// keeps that field stable.
	var hashes [2]string
	for i := range 2 {
		outBin := filepath.Join(t.TempDir(), "add_ints")
		d := &Driver{
			CacheDir: t.TempDir(),
			NoCache:  true,
		}
		if err := d.Build(src, outBin, "", ""); err != nil {
			t.Fatalf("build %d: %v", i+1, err)
		}
		h, err := sha256File(outBin)
		if err != nil {
			t.Fatalf("sha256 build %d: %v", i+1, err)
		}
		hashes[i] = h
	}

	if hashes[0] != hashes[1] {
		t.Fatalf("binary not reproducible:\nbuild1 sha256: %s\nbuild2 sha256: %s",
			hashes[0], hashes[1])
	}
	t.Logf("reproducible sha256: %s", hashes[0])
}

// sha256File returns the hex SHA-256 digest of the file at path.
func sha256File(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return fmt.Sprintf("%x", h.Sum(nil)), nil
}
