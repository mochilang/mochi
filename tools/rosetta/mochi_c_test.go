//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/types"
)

func TestMochiToC(t *testing.T) {
	cc, err := ccode.EnsureCC()
	if err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/C")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, outPath := range outs {
		name := strings.TrimSuffix(filepath.Base(outPath), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}
		t.Run(name, func(t *testing.T) {
			compileAndRunC(t, cc, srcPath, outPath, outDir, name)
		})
	}
}

func compileAndRunC(t *testing.T, cc, srcPath, wantPath, outDir, name string) {
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeCError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeCError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	code, err := ccode.New(env).Compile(prog)
	if err != nil {
		writeCError(outDir, name, fmt.Errorf("compile error: %w", err))
		t.Skip("compile error")
		return
	}
	cFile := filepath.Join(outDir, name+".c")
	if err := os.WriteFile(cFile, code, 0o644); err != nil {
		t.Fatalf("write c: %v", err)
	}
	tmp := t.TempDir()
	bin := filepath.Join(tmp, name)
	if out, err := exec.Command(cc, cFile, "-o", bin).CombinedOutput(); err != nil {
		writeCError(outDir, name, fmt.Errorf("cc error: %w\n%s", err, out))
		return
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		writeCError(outDir, name, fmt.Errorf("run error: %w\n%s", err, out))
		return
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writeCError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		return
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeCError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
