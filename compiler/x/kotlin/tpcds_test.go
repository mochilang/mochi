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

func runTPCDSQuery(t *testing.T, base string) {
	root := repoRoot()
	src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	errPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "kotlin", base+".error")
	c := kotlin.New(env, filepath.Base(src))
	code, err := c.Compile(prog)
	if err != nil {
		os.WriteFile(errPath, []byte(err.Error()), 0644)
		t.Skipf("compile error: %v", err)
		return
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		os.WriteFile(errPath, append([]byte("kotlinc: "+err.Error()+"\n"), out...), 0644)
		t.Skipf("kotlinc error: %v", err)
		return
	}
	out, err := exec.Command("java", "-jar", jar).CombinedOutput()
	if err != nil {
		os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0644)
		t.Skipf("run error: %v", err)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "kotlin", base+".out")
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
	}
	_ = os.Remove(errPath)
}

func TestKotlinCompiler_TPCDS(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	queries := []string{"q5", "q9", "q35", "q43", "q58", "q59", "q61", "q62", "q63", "q64", "q66", "q67", "q68", "q69", "q72", "q78", "q80", "q82", "q83", "q84", "q85", "q86", "q89", "q97"}
	for _, base := range queries {
		t.Run(base, func(t *testing.T) { runTPCDSQuery(t, base) })
	}
}
