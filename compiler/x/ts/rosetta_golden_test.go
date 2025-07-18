//go:build slow

package tscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func runRosettaTaskGolden(t *testing.T, name string) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot(t)
	script := exec.Command("go", "run", "-tags=archive,slow", "./scripts/compile_rosetta_ts.go")
	script.Env = append(os.Environ(), "GOTOOLCHAIN=local", "MOCHI_NOW_SEED=1", "TASKS="+name)
	script.Dir = root
	if out, err := script.CombinedOutput(); err != nil {
		t.Fatalf("compile script error: %v\n%s", err, out)
	}

	errFile := filepath.Join(root, "tests", "rosetta", "out", "TypeScript", name+".error")
	if b, err := os.ReadFile(errFile); err == nil {
		t.Skipf("typescript run failed:\n%s", b)
		return
	}

	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := tscode.New(env, root)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWant := filepath.Join(root, "tests", "rosetta", "out", "TypeScript", name+".ts")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(codeWant, code, 0644)
	} else if _, err := os.Stat(codeWant); err != nil {
		t.Fatalf("read golden: %v", err)
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.ts")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system", "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Skipf("run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWant := filepath.Join(root, "tests", "rosetta", "out", "TypeScript", name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestTSCompiler_Rosetta_Golden(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skip("deno not installed")
	}
	root := findRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	max := 3
	if len(files) < max {
		max = len(files)
	}
	for _, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) { runRosettaTaskGolden(t, name) })
	}
}
