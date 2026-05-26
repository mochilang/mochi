package build

import (
	"os"
	"testing"

	"mochi/transpiler3/c/toolchain/cosmocc"
)

// TestPhase13CosmoVendor is the MEP-45 Phase 13.1 gate. It verifies that
// the cosmocc package can locate or download a vendored cosmocc binary and
// that resolveCosmoCC picks it up without MOCHI_COSMOCC_PATH being set.
//
// Sub-tests:
//   - install_root: InstallRoot() returns a non-empty path.
//   - find_env_override: Find() respects MOCHI_COSMOCC_PATH.
//   - ensure_cached: if MOCHI_COSMOCC_VENDOR_TEST is set, Ensure() downloads
//     cosmocc to a temp dir and the binary exists afterwards.
//
// The ensure_cached sub-test only runs when MOCHI_COSMOCC_VENDOR_TEST=1 to
// avoid hitting the network in normal CI.
func TestPhase13CosmoVendor(t *testing.T) {
	t.Run("install_root", func(t *testing.T) {
		root, err := cosmocc.InstallRoot()
		if err != nil {
			t.Fatalf("InstallRoot: %v", err)
		}
		if root == "" {
			t.Fatal("InstallRoot returned empty string")
		}
		t.Logf("install root: %s", root)
	})

	t.Run("find_env_override", func(t *testing.T) {
		want := "/fake/path/to/cosmocc"
		t.Setenv("MOCHI_COSMOCC_PATH", want)
		got, err := cosmocc.Find()
		if err != nil {
			t.Fatalf("Find: %v", err)
		}
		if got != want {
			t.Fatalf("Find: want %q got %q", want, got)
		}
	})

	t.Run("find_vendor_dir_override", func(t *testing.T) {
		tmp := t.TempDir()
		t.Setenv("MOCHI_COSMOCC_DIR", tmp)
		t.Setenv("MOCHI_COSMOCC_PATH", "")

		// No binary present yet; Find should return empty (graceful degradation).
		got, err := cosmocc.Find()
		if err != nil {
			t.Fatalf("Find with empty vendor dir: %v", err)
		}
		if got != "" {
			t.Fatalf("Find with empty vendor dir: expected empty, got %q", got)
		}
	})

	t.Run("ensure_cached", func(t *testing.T) {
		if os.Getenv("MOCHI_COSMOCC_VENDOR_TEST") != "1" {
			t.Skip("set MOCHI_COSMOCC_VENDOR_TEST=1 to enable network download test")
		}
		tmp := t.TempDir()
		t.Setenv("MOCHI_COSMOCC_DIR", tmp)
		t.Setenv("MOCHI_COSMOCC_PATH", "")

		bin, err := cosmocc.Ensure()
		if err != nil {
			t.Fatalf("Ensure: %v", err)
		}
		if bin == "" {
			t.Fatal("Ensure returned empty path")
		}
		if _, err := os.Stat(bin); err != nil {
			t.Fatalf("binary not found after Ensure: %v", err)
		}
		t.Logf("cosmocc binary: %s", bin)
	})
}
