//go:build slow

package kotlin_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	kotlin "mochi/compiler/x/kotlin"
	"mochi/parser"
	"mochi/types"
)

func runJOBQuery(t *testing.T, base string) {
	root := repoRoot()
	src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := kotlin.New(env, filepath.Base(src))
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "kt", base+".kt")
	want, err := os.ReadFile(codeWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	got := stripHeader(bytes.TrimSpace(code))
	want = stripHeader(bytes.TrimSpace(want))
	if !bytes.Equal(got, want) {
		t.Errorf("generated code mismatch for %s.kt\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		t.Fatalf("kotlinc error: %v\n%s", err, out)
	}
	out, err := exec.Command("java", "-jar", jar).CombinedOutput()
	if err != nil {
		t.Fatalf("java run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "kt", base+".out")
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestKotlinCompiler_JOB(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	runJOBQuery(t, "q1")
}
