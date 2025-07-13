//go:build slow

package pascode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pascode "mochi/compiler/x/pascal"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPascalCompiler_JOB_Golden(t *testing.T) {
	fpc := ensureFPCQuick(t)
	root := testutil.FindRepoRoot(t)
	runQuery := func(q string) {
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
		codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "pas", q+".pas.out")
		wantCode, err := os.ReadFile(codeWant)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		strip := func(b []byte) []byte {
			if i := bytes.IndexByte(b, '\n'); i >= 0 {
				return bytes.TrimSpace(b[i+1:])
			}
			return bytes.TrimSpace(b)
		}
		got := strip(code)
		want := strip(wantCode)
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".pas.out", got, want)
		}
		dir := t.TempDir()
		srcFile := filepath.Join(dir, "main.pas")
		if err := os.WriteFile(srcFile, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		exe := filepath.Join(dir, "main")
		if out, err := exec.Command(fpc, srcFile, "-o"+exe).CombinedOutput(); err != nil {
			t.Skipf("fpc error: %v\n%s", err, out)
		}
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			t.Skipf("run error: %v\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "pas", q+".out")
		wantOut, err := os.ReadFile(outWant)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
	for _, q := range []string{"q1", "q2", "q3", "q4", "q5"} {
		t.Run(q, func(t *testing.T) { runQuery(q) })
	}
}
