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

func runTPCHQuery(t *testing.T, base string) {
	root := repoRoot()
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := kotlin.New(env, src)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "kt", base+".kt.out")
	if want, err := os.ReadFile(codeWant); err == nil {
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("generated code mismatch for %s.kt.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(want))
		}
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
	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "kt", base+".out")
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestKotlinCompiler_TPCH(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
       for _, base := range []string{"q1", "q2"} {
               t.Run(base, func(t *testing.T) { runTPCHQuery(t, base) })
       }
}
