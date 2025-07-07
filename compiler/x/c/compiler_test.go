//go:build slow

package c_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	c "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/types"
)

func ensureCC(t *testing.T) string {
	if env := os.Getenv("CC"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	for _, cc := range []string{"cc", "gcc", "clang"} {
		if p, err := exec.LookPath(cc); err == nil {
			return p
		}
	}
	t.Skip("C compiler not found")
	return ""
}

func repoRoot() string {
	_, file, _, _ := runtime.Caller(0)
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}

func compileAndRun(t *testing.T, src string, ccPath string) {
	abs := filepath.Join(repoRoot(), src)
	prog, err := parser.Parse(abs)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	comp := c.New(env)
	code, err := comp.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	outDir := filepath.Join(repoRoot(), "tests", "machine", "x", "c")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir error: %v", err)
	}
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	cfile := filepath.Join(outDir, base+".c")
	if err := os.WriteFile(cfile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	bin := filepath.Join(outDir, base)
	if out, err := exec.Command(ccPath, cfile, "-o", bin).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0644)
		t.Fatalf("cc error: %v", err)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0644)
		t.Fatalf("run error: %v", err)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), out, 0644); err != nil {
		t.Fatalf("write output: %v", err)
	}
}

func TestCCompiler_BasicPrograms(t *testing.T) {
	cc := ensureCC(t)
	cases := []string{
		filepath.Join("tests", "vm", "valid", "print_hello.mochi"),
		filepath.Join("tests", "vm", "valid", "while_loop.mochi"),
	}
	for _, src := range cases {
		t.Run(filepath.Base(src), func(t *testing.T) {
			compileAndRun(t, src, cc)
		})
	}
}
