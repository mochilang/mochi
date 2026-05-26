package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase14CassetteRecord is the MEP-45 Phase 14.5 gate. It verifies that:
//  1. When MOCHI_LLM_CASSETTE_DIR is set (playback mode), the binary returns
//     the cassette response and does NOT write to MOCHI_LLM_CASSETTE_RECORD
//     (playback takes priority).
//  2. When only MOCHI_LLM_CASSETTE_RECORD is set (no playback dir), the binary
//     calls the live dispatch (which returns "" in stub mode), writes the
//     response to CASSETTE_RECORD/<hash>.txt, and returns the response.
//
// The gate does NOT require a live API key. Sub-test 2 uses the stub live path
// (no MOCHI_LLM_HAVE_CURL) which returns "" and still triggers the write.
//
// Skip conditions:
//   - Windows: no C toolchain in CI
func TestPhase14CassetteRecord(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}

	root := repoRoot(t)
	fixture := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "llm", "generate_text")
	src := filepath.Join(fixture, "generate_text.mochi")
	cassetteDir := filepath.Join(fixture, "cassette")
	expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	// Build in stub mode (no extra flags).
	outBin := filepath.Join(t.TempDir(), "generate_text_record")
	d := &Driver{
		CacheDir: t.TempDir(),
		NoCache:  true,
	}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}

	// Sub-test 1: playback takes priority when CASSETTE_DIR is set.
	// Even with CASSETTE_RECORD also set, no file should be written to the record dir.
	t.Run("playback_priority", func(t *testing.T) {
		recordDir := t.TempDir()
		cmd := exec.Command(outBin)
		cmd.Env = append(os.Environ(),
			"MOCHI_LLM_CASSETTE_DIR="+cassetteDir,
			"MOCHI_LLM_CASSETTE_RECORD="+recordDir,
		)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("run with cassette+record: %v\nstdout: %q", err, stdout.String())
		}
		// Response must come from the cassette (playback wins).
		if got := stdout.String(); got != string(expect) {
			t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(expect), got)
		}
		// Record dir must be empty (playback fired, not live dispatch).
		entries, _ := os.ReadDir(recordDir)
		if len(entries) != 0 {
			t.Fatalf("record dir should be empty when playback fires, got %d files", len(entries))
		}
	})

	// Sub-test 2: record mode writes the live response to the cassette dir.
	// With no CASSETTE_DIR and no API key, stub returns "" and that gets recorded.
	t.Run("record_write", func(t *testing.T) {
		recordDir := t.TempDir()
		env := filterEnv(os.Environ(),
			"MOCHI_LLM_CASSETTE_DIR", "OPENAI_API_KEY",
			"ANTHROPIC_API_KEY", "GOOGLE_API_KEY",
		)
		env = append(env, "MOCHI_LLM_CASSETTE_RECORD="+recordDir)

		cmd := exec.Command(outBin)
		cmd.Env = env
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		_ = cmd.Run() // may exit non-zero; we check for the file

		// A cassette file must have been written regardless of the live result.
		entries, err := os.ReadDir(recordDir)
		if err != nil {
			t.Fatalf("ReadDir record dir: %v", err)
		}
		if len(entries) == 0 {
			t.Fatalf("record mode did not write any cassette file (stderr: %q)", stderr.String())
		}
		// Verify the written file exists and has a .txt extension.
		if ext := filepath.Ext(entries[0].Name()); ext != ".txt" {
			t.Fatalf("recorded file has unexpected extension: %q", entries[0].Name())
		}
		t.Logf("recorded cassette: %s", entries[0].Name())
	})
}
