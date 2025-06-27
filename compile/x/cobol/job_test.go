//go:build slow

package cobolcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/compile/x/testutil"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func TestCobolCompiler_JOB_Q1(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
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

	modRoot, _ := mod.FindRoot(filepath.Dir(src))
	interp := interpreter.New(prog, env, modRoot)
	if err := interp.Test(); err != nil {
		t.Fatalf("tests failed: %v", err)
	}
	interp = interpreter.New(prog, env, modRoot)
	var wantBuf bytes.Buffer
	interp.Env().SetWriter(&wantBuf)
	if err := interp.Run(); err != nil {
		t.Fatalf("run error: %v", err)
	}
	want := bytes.TrimSpace(wantBuf.Bytes())

	code, err := cobolcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	tmp := t.TempDir()
	cobFile := filepath.Join(tmp, "main.cob")
	if err := os.WriteFile(cobFile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(tmp, "main")
	if out, err := exec.Command("cobc", "-free", "-x", cobFile, "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("cobc error: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	if !bytes.Equal(got, want) {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}
