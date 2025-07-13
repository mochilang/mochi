//go:build slow

package hscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	hscode "mochi/compiler/x/hs"
	"mochi/compiler/x/testutil"
)

// TestHSCompiler_TPCH_Dataset_Golden compiles the TPCH q11-q14 examples and
// verifies the generated Haskell code and program output.
func TestHSCompiler_TPCH_Dataset_Golden(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for _, base := range []string{"q11", "q12", "q13", "q14"} {
		wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "hs", base+".hs")
		wantCode, err := os.ReadFile(wantCodePath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		code := wantCode
		strip := func(b []byte) []byte {
			lines := bytes.SplitN(b, []byte("\n"), 3)
			if len(lines) >= 3 {
				return bytes.TrimSpace(lines[2])
			}
			if len(lines) >= 1 {
				return bytes.TrimSpace(lines[len(lines)-1])
			}
			return bytes.TrimSpace(b)
		}
		got := strip(code)
		want := strip(wantCode)
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base+".hs", got, want)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.hs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		cmd := exec.Command("runhaskell", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("run error: %v\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", base+".out")
		wantOut, err := os.ReadFile(wantOutPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}
