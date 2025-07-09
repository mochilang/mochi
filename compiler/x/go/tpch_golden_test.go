//go:build slow

package gocode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	gocode "mochi/compiler/x/go"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	durRE := regexp.MustCompile(`\([0-9]+(\.[0-9]+)?(ns|µs|ms|s)\)`)
	out = durRE.ReplaceAllString(out, "(X)")
	out = strings.TrimSpace(out)
	return []byte(out)
}

func TestGoCompiler_TPCH_Q1(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", "q1.go.out")
	wantCode, err := os.ReadFile(wantCodePath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	got := bytes.TrimSpace(code)
	if !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("go", "run", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	if idx := bytes.IndexByte(gotOut, '\n'); idx >= 0 {
		gotOut = gotOut[:idx]
	}

	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatalf("vm compile error: %v", err)
	}
	var vmBuf bytes.Buffer
	m := vm.New(p, &vmBuf)
	if err := m.Run(); err != nil {
		t.Fatalf("vm run error: %v", err)
	}
	vmOut := bytes.TrimSpace(vmBuf.Bytes())
	if idx := bytes.IndexByte(vmOut, '\n'); idx >= 0 {
		vmOut = vmOut[:idx]
	}
	if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, vmOut)) {
		t.Errorf("vm mismatch\n\n--- Go ---\n%s\n\n--- VM ---\n%s\n", gotOut, vmOut)
	}

	wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", "q1.out")
	wantOut, err := os.ReadFile(wantOutPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	wantOut = bytes.TrimSpace(wantOut)
	if idx := bytes.IndexByte(wantOut, '\n'); idx >= 0 {
		wantOut = wantOut[:idx]
	}
	if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, wantOut)
	}
}

func TestGoCompiler_TPCH_Q2(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q2.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCodePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", "q2.go.out")
	wantCode, err := os.ReadFile(wantCodePath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	got := bytes.TrimSpace(code)
	if !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q2.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("go", "run", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run error: %v\n%s", err, out)
	}
	gotOut := bytes.TrimSpace(out)
	if idx := bytes.IndexByte(gotOut, '\n'); idx >= 0 {
		gotOut = gotOut[:idx]
	}

	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatalf("vm compile error: %v", err)
	}
	var vmBuf bytes.Buffer
	m := vm.New(p, &vmBuf)
	if err := m.Run(); err != nil {
		t.Fatalf("vm run error: %v", err)
	}
	vmOut := bytes.TrimSpace(vmBuf.Bytes())
	if idx := bytes.IndexByte(vmOut, '\n'); idx >= 0 {
		vmOut = vmOut[:idx]
	}
	if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, vmOut)) {
		t.Errorf("vm mismatch\n\n--- Go ---\n%s\n\n--- VM ---\n%s\n", gotOut, vmOut)
	}

	wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", "q2.out")
	wantOut, err := os.ReadFile(wantOutPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	wantOut = bytes.TrimSpace(wantOut)
	if idx := bytes.IndexByte(wantOut, '\n'); idx >= 0 {
		wantOut = wantOut[:idx]
	}
	if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, wantOut)) {
		t.Errorf("output mismatch for q2.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, wantOut)
	}
}
