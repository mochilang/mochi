package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase6UTF8 is the MEP-45 Phase 6.6 gate for UTF-8 validation on
// readFile. It has two sub-gates:
//
//   - valid_files: 5 fixtures that write ASCII content to /tmp and read
//     it back. The UTF-8 validator must not reject valid ASCII, and the
//     file content must print correctly.
//
//   - invalid_utf8: one fixture that writes a byte sequence with an
//     invalid UTF-8 lead byte (0xFF) then calls readFile. The program
//     must exit with MOCHI_ERR_PARSE (exit code 2).
func TestPhase6UTF8(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}

	t.Run("valid_files", func(t *testing.T) {
		runFixtureSuite(t, "utf8")
	})

	t.Run("invalid_utf8", func(t *testing.T) {
		const src = `let s = readFile("/tmp/mochi_utf8_invalid.txt")
print(s)
`
		srcFile := filepath.Join(t.TempDir(), "utf8_invalid.mochi")
		if err := os.WriteFile(srcFile, []byte(src), 0o644); err != nil {
			t.Fatalf("write source: %v", err)
		}

		outBin := filepath.Join(t.TempDir(), "utf8_invalid")
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(srcFile, outBin, "", ""); err != nil {
			t.Fatalf("Driver.Build: %v", err)
		}

		// Write a file with an invalid UTF-8 byte sequence before running the binary.
		// 0xFF is never valid in UTF-8 (not a lead byte for any valid sequence).
		if err := os.WriteFile("/tmp/mochi_utf8_invalid.txt", []byte{0x68, 0x69, 0xFF, 0x0A}, 0o644); err != nil {
			t.Fatalf("write invalid utf-8 file: %v", err)
		}

		cmd := exec.Command(outBin)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = os.Stderr
		err := cmd.Run()
		if err == nil {
			t.Fatalf("expected exit error for invalid UTF-8, got nil (stdout: %q)", stdout.String())
		}
		exitErr, ok := err.(*exec.ExitError)
		if !ok {
			t.Fatalf("expected *exec.ExitError, got %T: %v", err, err)
		}
		const wantExit = 2 // MOCHI_ERR_PARSE
		if got := exitErr.ExitCode(); got != wantExit {
			t.Fatalf("exit code: want %d, got %d", wantExit, got)
		}
		if stdout.Len() != 0 {
			t.Fatalf("expected empty stdout for invalid UTF-8, got %q", stdout.String())
		}
	})
}
