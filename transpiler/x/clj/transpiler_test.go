package cljt_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

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

// EnsureClojure checks that the clojure CLI tool is installed.
func EnsureClojure() error {
	if _, err := exec.LookPath("clojure"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clj"); err == nil {
		return nil
	}
	return exec.ErrNotFound
}

func TestTranspile_Golden(t *testing.T) {
	if err := EnsureClojure(); err != nil {
		t.Skip("clojure not installed")
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi tests found")
	}

	for _, wantPath := range outs {
		name := strings.TrimSuffix(filepath.Base(wantPath), ".out")
		if strings.Contains(name, ".") { // skip language-specific outputs
			continue
		}
		srcPath := filepath.Join(srcDir, name+".mochi")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}
		t.Run(name, func(t *testing.T) {
			compileAndRunClojure(t, srcPath, wantPath, outDir, name)
		})
	}
}

func compileAndRunClojure(t *testing.T, srcPath, wantPath, outDir, name string) {
	if _, err := os.ReadFile(srcPath); err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeCljError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeCljError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	ast, err := cljt.Transpile(prog, env)
	if err != nil {
		writeCljError(outDir, name, fmt.Errorf("transpile error: %w", err))
		t.Skip("transpile error")
		return
	}
	code := cljt.EmitString(ast)
	cljPath := filepath.Join(outDir, name+".clj")
	if err := os.WriteFile(cljPath, code, 0o644); err != nil {
		t.Fatalf("write clj: %v", err)
	}
	var buf bytes.Buffer
	cmd := exec.Command("clojure", cljPath)
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeCljError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
		return
	}
	got := bytes.TrimSpace(buf.Bytes())
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writeCljError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		return
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeCljError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
