//go:build slow

package rkt_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/parser"
	rkt "mochi/transpiler/x/rkt"
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

func TestTranspile_Golden(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rkt")
	os.MkdirAll(outDir, 0o755)
	names := []string{
		"print_hello",
		"string_concat",
		"let_and_print",
		"var_assignment",
		"basic_compare",
		"math_ops",
		"string_compare",
		"if_else",
		"if_then_else",
		"unary_neg",
	}
	for _, name := range names {
		src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type %s: %v", name, errs[0])
		}
		ast, err := rkt.Transpile(prog, env)
		if err != nil {
			t.Fatalf("transpile %s: %v", name, err)
		}
		var buf bytes.Buffer
		if err := rkt.Emit(&buf, ast); err != nil {
			t.Fatalf("emit %s: %v", name, err)
		}
		rktFile := filepath.Join(outDir, name+".rkt")
		if err := os.WriteFile(rktFile, buf.Bytes(), 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
		cmd := exec.Command("racket", rktFile)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		out, err := cmd.CombinedOutput()
		trimmed := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("run %s: %v", name, err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
		if err != nil {
			t.Fatalf("read want %s: %v", name, err)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(trimmed, want) {
			t.Errorf("%s output mismatch:\nGot: %s\nWant: %s", name, trimmed, want)
		}
	}
}
