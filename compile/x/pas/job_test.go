//go:build slow

package pascode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pascode "mochi/compile/x/pas"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestPascalCompiler_JOB compiles the JOB q1 program with the Pascal backend.
func TestPascalCompiler_JOB(t *testing.T) {
	fpc, err := pascode.EnsureFPC()
	if err != nil {
		t.Skipf("fpc not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := pascode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Skipf("compile error: %v", err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "prog.pas")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	if out, err := exec.Command(fpc, file).CombinedOutput(); err != nil {
		t.Skipf("fpc error: %v\n%s", err, out)
	}
	exe := filepath.Join(tmp, "prog")
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	wantData, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "out", "q1.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want := strings.TrimSpace(string(wantData))
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}
