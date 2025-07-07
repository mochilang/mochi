package smalltalk_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	st "mochi/compiler/x/smalltalk"
)

// TestCompilePrograms compiles each Mochi program under tests/vm/valid to
// Smalltalk and attempts to run it with the GNU Smalltalk interpreter (gst).
// Generated source and outputs are written under tests/machine/x/st.
func TestCompilePrograms(t *testing.T) {
	// locate test programs relative to the repository root
	_, file, _, _ := runtime.Caller(0)
	root := filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}

	c := st.New()

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			code, err := c.Compile(src)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}

			outDir := filepath.Join(root, "tests", "machine", "x", "st")
			os.MkdirAll(outDir, 0755)
			stFile := filepath.Join(outDir, name+".st")
			if err := os.WriteFile(stFile, code, 0644); err != nil {
				t.Fatalf("write st: %v", err)
			}

			cmd := exec.Command("gst", stFile)
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			runErr := cmd.Run()
			if runErr == nil {
				if err := os.WriteFile(filepath.Join(outDir, name+".out"), stdout.Bytes(), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
				return
			}
			// on error
			var buf strings.Builder
			buf.WriteString(runErr.Error())
			if stderr.Len() > 0 {
				buf.WriteString(": ")
				buf.Write(stderr.Bytes())
			}
			errPath := filepath.Join(outDir, name+".error")
			os.WriteFile(errPath, []byte(buf.String()), 0644)
		})
	}
}
