//go:build slow

package rktcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rktcode "mochi/compile/x/rkt"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_TPCH_Golden(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for _, q := range []string{"q1", "q2"} {
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type error: %v", errs[0])
		}
		code, err := rktcode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rkt", q+".rkt.out"))
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
		}
		tmp := t.TempDir()
		file := filepath.Join(tmp, "main.rkt")
		if err := os.WriteFile(file, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		cmd := exec.Command("racket", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Skipf("%s failed to run: %v", q, err)
		}
		gotRun := strings.TrimSpace(string(out))
		wantRunBytes, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "rkt", q+".out"))
		if err != nil {
			t.Fatalf("read output golden: %v", err)
		}
		wantRun := strings.TrimSpace(string(wantRunBytes))
		if gotRun != wantRun {
			t.Errorf("%s runtime mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotRun, wantRun)
		}
	}
}
