//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateVM() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func repoRootVM(t *testing.T) string {
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

// runVMGolden compiles the Mochi source file under tests/vm/valid, executes the
// generated TypeScript with Deno and compares the output with the VM golden
// file. Generated code, output and any error logs are stored under
// tests/machine/x/ts.
func runVMGolden(t *testing.T, src string) {
	root := repoRootVM(t)
	name := filepath.Base(src[:len(src)-len(filepath.Ext(src))])
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("MOCHI_HEADER_TIME")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "ts")
	codeWant := filepath.Join(outDir, name+".ts")
	outWant := filepath.Join(outDir, name+".out")
	errFile := filepath.Join(outDir, name+".error")

	code, err := tscode.New(env, filepath.Dir(src)).Compile(prog)
	if err != nil {
		_ = os.WriteFile(errFile, []byte("compile: "+err.Error()), 0644)
		return
	}

	if shouldUpdateVM() {
		if err := os.WriteFile(codeWant, code, 0644); err != nil {
			t.Fatalf("write golden code: %v", err)
		}
	} else if want, err := os.ReadFile(codeWant); err == nil {
		got := bytes.TrimSpace(code)
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.ts\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
		}
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.ts")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write temp code: %v", err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	cmd.Dir = filepath.Join(filepath.Dir(src), "..")
	if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(inData)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("run: %v\n%s", err, out)), 0644)
		return
	}
	gotOut := bytes.TrimSpace(out)
	_ = os.Remove(errFile)
	if shouldUpdateVM() {
		if err := os.WriteFile(outWant, append(gotOut, '\n'), 0644); err != nil {
			t.Fatalf("write golden out: %v", err)
		}
	} else if wantOut, err := os.ReadFile(outWant); err == nil {
		if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}

func TestTSCompiler_VMValid_Golden(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := repoRootVM(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	for _, f := range files {
		name := filepath.Base(f)
		t.Run(name, func(t *testing.T) { runVMGolden(t, f) })
	}
}
