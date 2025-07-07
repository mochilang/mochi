//go:build archived && slow

package pascode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pascode "mochi/archived/x/pas"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPascalCompiler_JOB_Golden(t *testing.T) {
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	// compile and run the first ten JOB queries
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type error: %v", errs[0])
		}
		code, err := pascode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "pas", q+".pas.out")
		wantCode, err := os.ReadFile(wantPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		got := bytes.TrimSpace(code)
		if !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.pas.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "prog.pas")
		if err := os.WriteFile(file, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		if out, err := exec.Command(fpc, file).CombinedOutput(); err != nil {
			t.Fatalf("fpc error: %v\n%s", err, out)
		}
		exe := filepath.Join(dir, "prog")
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			t.Fatalf("run error: %v\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "pas", q+".out")
		wantOut, err := os.ReadFile(outWantPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}
