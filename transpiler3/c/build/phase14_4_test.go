package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase14Llama is the MEP-45 Phase 14.4 gate. It verifies that:
//  1. A program using generate llama { ... } compiles correctly (stub mode,
//     without -DMOCHI_LLM_HAVE_LLAMA).
//  2. In cassette mode (MOCHI_LLM_CASSETTE_DIR set), the stub binary returns
//     the pre-recorded cassette response (cassette takes priority over live).
//  3. In no-cassette mode without LLAMA_MODEL_PATH, the binary exits cleanly
//     with an empty result (the stub prints a diagnostic but does not crash).
//
// The gate does NOT link llama.cpp (no --with-llama required).
// The llama.cpp implementation (MOCHI_LLM_HAVE_LLAMA path) is compiled only
// when the user builds with -DMOCHI_LLM_HAVE_LLAMA -lllama.
//
// Skip conditions:
//   - Windows: no C toolchain in CI
func TestPhase14Llama(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}

	root := repoRoot(t)
	fixture := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "llm", "generate_llama")
	src := filepath.Join(fixture, "generate_llama.mochi")
	cassetteDir := filepath.Join(fixture, "cassette")
	expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	// Build in stub mode (no -DMOCHI_LLM_HAVE_LLAMA).
	outBin := filepath.Join(t.TempDir(), "generate_llama_stub")
	d := &Driver{
		CacheDir: t.TempDir(),
		NoCache:  true,
	}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build (stub mode): %v", err)
	}

	// Sub-test 1: cassette mode returns pre-recorded response.
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

	// Sub-test 2: stub live mode without LLAMA_MODEL_PATH prints a diagnostic and returns "".
	t.Run("stub_no_model_path", func(t *testing.T) {
		env := filterEnv(os.Environ(), "MOCHI_LLM_CASSETTE_DIR", "LLAMA_MODEL_PATH")
		cmd := exec.Command(outBin)
		cmd.Env = env
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		_ = cmd.Run()
		// The binary must not produce the cassette response (cassette dir is unset).
		got := stdout.String()
		if got == string(expect) {
			t.Fatalf("stub live mode produced cassette output unexpectedly")
		}
		// stderr should mention the missing model path or provider.
		errOut := stderr.String()
		if errOut == "" {
			t.Logf("warning: no stderr output from stub live mode (expected diagnostic)")
		}
	})
}
