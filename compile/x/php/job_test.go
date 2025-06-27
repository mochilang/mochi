//go:build slow

package phpcode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	phpcode "mochi/compile/x/php"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestPHPCompiler_JOBQ1(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
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
	code, err := phpcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "main.php")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	// compare generated code
	codeWant, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "php", "q1.php.out"))
	if err == nil {
		if string(code) != string(codeWant) {
			t.Fatalf("generated code mismatch")
		}
	}
	cmd := exec.Command("php", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("php run error: %v\n%s", err, out)
	}
	got := string(out)
	wantData, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "php", "q1.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want := string(wantData)
	if got != want {
		t.Fatalf("unexpected output\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}
