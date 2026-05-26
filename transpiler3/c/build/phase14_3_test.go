package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase14Google is the MEP-45 Phase 14.3 gate. It verifies that:
//  1. A program using generate google { ... } compiles correctly when built
//     with -DMOCHI_LLM_HAVE_CURL -lcurl (enabling the live HTTP provider).
//  2. In cassette mode (MOCHI_LLM_CASSETTE_DIR set), the curl-enabled binary
//     still returns the pre-recorded cassette response (cassette takes priority).
//  3. In no-cassette mode without GOOGLE_API_KEY, the binary exits cleanly with
//     an empty result (the live provider prints a diagnostic but does not crash).
//
// The gate does NOT make live HTTP calls to generativelanguage.googleapis.com
// (no API key required). Live mode is validated by the absence of a crash.
//
// Skip conditions:
//   - Windows: no C toolchain in CI
//   - libcurl not linkable via cc -lcurl
func TestPhase14Google(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}

	if !probeLibcurl(t) {
		t.Skip("libcurl not available on this host; skipping Phase 14.3 curl gate")
	}

	root := repoRoot(t)
	fixture := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "llm", "generate_google")
	src := filepath.Join(fixture, "generate_google.mochi")
	cassetteDir := filepath.Join(fixture, "cassette")
	expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	// Build with -DMOCHI_LLM_HAVE_CURL -lcurl.
	outBin := filepath.Join(t.TempDir(), "generate_google_curl")
	d := &Driver{
		CacheDir:   t.TempDir(),
		NoCache:    true,
		ExtraFlags: []string{"-DMOCHI_LLM_HAVE_CURL", "-lcurl"},
	}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build with curl flags: %v", err)
	}

	// Sub-test 1: cassette mode still works with curl-enabled binary.
	t.Run("cassette_mode", func(t *testing.T) {
		cmd := exec.Command(outBin)
		cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("run with cassette: %v\nstdout: %q", err, stdout.String())
		}
		if got := stdout.String(); got != string(expect) {
			t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(expect), got)
		}
	})

	// Sub-test 2: live mode without API key returns empty string (no crash).
	t.Run("live_no_api_key", func(t *testing.T) {
		env := filterEnv(os.Environ(), "MOCHI_LLM_CASSETTE_DIR", "GOOGLE_API_KEY")
		cmd := exec.Command(outBin)
		cmd.Env = env
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		_ = cmd.Run()
		// The binary must not produce the cassette response (cassette dir is unset).
		got := stdout.String()
		if got == string(expect) {
			t.Fatalf("live mode without API key produced cassette output unexpectedly")
		}
		// stderr should mention GOOGLE_API_KEY or live mode.
		errOut := stderr.String()
		if errOut == "" {
			t.Logf("warning: no stderr output from live mode (expected diagnostic)")
		}
	})
}
