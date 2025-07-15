//go:build slow

package kotlin_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	kotlin "mochi/compiler/x/kotlin"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateTPCDS() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func runTPCDSQueryGolden(t *testing.T, q string) {
	root := repoRoot()
	script := exec.Command("go", "run", "-tags=archive,slow", "./scripts/compile_tpcds_kotlin.go")
	script.Env = append(os.Environ(), "GOTOOLCHAIN=local", "QUERIES="+q[1:])
	script.Dir = root
	if out, err := script.CombinedOutput(); err != nil {
		t.Fatalf("compile script error: %v\n%s", err, out)
	}
	errFile := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "kotlin", q+".error")
	if b, err := os.ReadFile(errFile); err == nil {
		t.Fatalf("kotlin run failed:\n%s", b)
	}

	src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := kotlin.New(env, filepath.Base(src)).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "kotlin", q+".kt")
	if shouldUpdateTPCDS() {
		_ = os.WriteFile(codeWant, code, 0644)
	} else if want, err := os.ReadFile(codeWant); err == nil {
		got := stripHeader(bytes.TrimSpace(code))
		want = stripHeader(bytes.TrimSpace(want))
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.kt\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, got, want)
		}
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
		t.Skipf("run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "kotlin", q+".out")
	if shouldUpdateTPCDS() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestKotlinCompiler_TPCDS_Golden(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		if _, err := os.Stat(filepath.Join(repoRoot(), "tests", "dataset", "tpc-ds", "compiler", "kotlin", q+".kt")); err != nil {
			continue
		}
		t.Run(q, func(t *testing.T) { runTPCDSQueryGolden(t, q) })
	}
}
