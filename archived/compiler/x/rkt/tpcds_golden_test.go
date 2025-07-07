//go:build archived && slow

package rktcode_test

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	rktcode "mochi/archived/x/rkt"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_TPCDS_Golden(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	q := "q1"
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
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
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rkt", q+".rkt.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
	}
	gotRunBytes, err := compileRun(t, src)
	if err != nil {
		t.Fatalf("%s: %v", q, err)
	}
	gotRun := string(bytes.TrimSpace(gotRunBytes))
	wantRunBytes, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "rkt", q+".out"))
	if err != nil {
		t.Fatalf("read output golden: %v", err)
	}
	wantRun := strings.TrimSpace(string(wantRunBytes))
	if gotRun != wantRun {
		t.Errorf("%s runtime mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotRun, wantRun)
	}
}
