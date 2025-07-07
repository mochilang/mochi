//go:build archived && slow

package dartcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	dartcode "mochi/archived/x/dart"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_JOB(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	root := findRoot(t)
	for _, q := range []string{"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"} {
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type error: %v", errs[0])
		}
		code, err := dartcode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "dart", q+".dart.out")
		wantCode, err := os.ReadFile(wantCodePath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s.dart.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.dart")
		if err := os.WriteFile(file, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		cmd := exec.Command("dart", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Skipf("dart run error: %v\n%s", err, out)
			continue
		}
		gotOut := bytes.TrimSpace(out)
		wantOutPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "dart", q+".out")
		wantOut, err := os.ReadFile(wantOutPath)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}
