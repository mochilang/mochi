//go:build archived && slow

package stcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	stcode "mochi/archived/x/st"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSTCompiler_JOBQueries(t *testing.T) {
	if err := stcode.EnsureSmalltalk(); err != nil {
		t.Skipf("smalltalk not installed: %v", err)
	}
	t.Skip("JOB queries not yet executing correctly under Smalltalk")

	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := stcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}

			codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "st", q+".st.out")
			if wantCode, err := os.ReadFile(codeWantPath); err == nil {
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s.st.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
				}
			}

			dir := t.TempDir()
			file := filepath.Join(dir, "main.st")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("gst", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("gst error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "st", q+".out")
			if wantOut, err := os.ReadFile(outWantPath); err == nil {
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
