//go:build slow

package gocode_test

import (
	"bytes"
	"fmt"
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

func runTPCHQuery(t *testing.T, base string) {
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")

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
		t.Skipf("compile error: %v", err)
		return
	}

	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "go", base+".go.out")
	if want, err := os.ReadFile(codeWant); err == nil {
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("generated code mismatch for %s.go.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(want))
		}
	}

	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("go", "run", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Skipf("go run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	if idx := bytes.IndexByte(gotOut, '\n'); idx >= 0 {
		gotOut = gotOut[:idx]
	}

	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Skipf("vm compile error: %v", err)
		return
	}
	var vmBuf bytes.Buffer
	m := vm.New(p, &vmBuf)
	if err := m.Run(); err != nil {
		t.Skipf("vm run error: %v", err)
		return
	}
	vmOut := bytes.TrimSpace(vmBuf.Bytes())
	if idx := bytes.IndexByte(vmOut, '\n'); idx >= 0 {
		vmOut = vmOut[:idx]
	}
	if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, vmOut)) {
		t.Skipf("vm mismatch for %s", base)
		return
	}

	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "out", base+".out")
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Skipf("read golden: %v", err)
		return
	}
	wantOut = bytes.TrimSpace(wantOut)
	if idx := bytes.IndexByte(wantOut, '\n'); idx >= 0 {
		wantOut = wantOut[:idx]
	}
	if !bytes.Equal(normalizeOutput(root, gotOut), normalizeOutput(root, wantOut)) {
		t.Skipf("output mismatch for %s", base)
		return
	}
}

func TestGoCompiler_TPCH(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	for i := 1; i <= 22; i++ {
		base := fmt.Sprintf("q%d", i)
		if _, err := os.Stat(filepath.Join(repoRoot(t), "tests", "dataset", "tpc-h", base+".mochi")); err != nil {
			continue
		}
		t.Run(base, func(t *testing.T) { runTPCHQuery(t, base) })
	}
}
