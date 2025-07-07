//go:build slow

package cobol_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	cobol "mochi/compiler/x/cobol"
	"mochi/parser"
	"mochi/types"
)

func ensureCobc(t *testing.T) string {
	if p, err := exec.LookPath("cobc"); err == nil {
		return p
	}
	t.Skip("COBOL compiler not found")
	return ""
}

func repoRoot() string {
	_, file, _, _ := runtime.Caller(0)
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}

func compileAndRun(t *testing.T, src, cobcPath string) {
	abs := filepath.Join(repoRoot(), src)
	prog, err := parser.Parse(abs)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	comp := cobol.New(env)
	code, err := comp.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	outDir := filepath.Join(repoRoot(), "tests", "machine", "x", "cobol")
	os.MkdirAll(outDir, 0755)
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	srcFile := filepath.Join(outDir, base+".cob")
	os.WriteFile(srcFile, code, 0644)
	bin := filepath.Join(outDir, base)
	if out, err := exec.Command(cobcPath, "-x", "-o", bin, srcFile).CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0644)
		t.Fatalf("cobc error: %v", err)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, base+".error"), out, 0644)
		t.Fatalf("run error: %v", err)
	}
	os.WriteFile(filepath.Join(outDir, base+".out"), out, 0644)
}

func TestCobolCompiler_BasicPrograms(t *testing.T) {
	cobc := ensureCobc(t)
	cases := []string{
		filepath.Join("tests", "vm", "valid", "print_hello.mochi"),
		filepath.Join("tests", "vm", "valid", "while_loop.mochi"),
	}
	for _, src := range cases {
		t.Run(filepath.Base(src), func(t *testing.T) {
			compileAndRun(t, src, cobc)
		})
	}
}
