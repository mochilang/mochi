//go:build slow

package pl_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pl "mochi/compiler/x/pl"
	"mochi/parser"
)

func TestPrologCompiler(t *testing.T) {
	files := []string{
		"print_hello.mochi",
		"let_and_print.mochi",
		"var_assignment.mochi",
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "pl")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	for _, f := range files {
		src := filepath.Join(srcDir, f)
		base := strings.TrimSuffix(f, ".mochi")
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			code, err := pl.New().Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codePath := filepath.Join(outDir, base+".pl")
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("swipl", "-q", codePath)
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			err = cmd.Run()
			if err != nil {
				errPath := filepath.Join(outDir, base+".error")
				msg := stderr.String()
				os.WriteFile(errPath, []byte(msg), 0644)
				t.Fatalf("swipl error: %v", err)
			}
			outPath := filepath.Join(outDir, base+".out")
			os.WriteFile(outPath, stdout.Bytes(), 0644)
		})
	}
}

func findRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
