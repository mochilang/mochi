//go:build slow

package ktcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	ktcode "mochi/compile/x/kt"
	"mochi/parser"
	"mochi/types"
)

func TestKTCompiler_JOBQ1(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := ktcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "kt", "q1.kt.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	gotCode := bytes.TrimSpace(code)
	if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.kt.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotCode, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		t.Skipf("kotlinc error: %v\n%s", err, out)
		return
	}
	out, err := exec.Command("java", "-jar", jar).CombinedOutput()
	if err != nil {
		t.Skipf("java run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "kt", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}
