//go:build slow

package racket_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	rack "mochi/compiler/x/racket"
	"mochi/parser"
)

func TestRacketCompiler(t *testing.T) {
	if err := rack.EnsureRacket(); err != nil {
		t.Skipf("racket not available: %v", err)
	}
	examples := []string{"print_hello", "for_loop", "typed_let"}
	for _, name := range examples {
		t.Run(name, func(t *testing.T) {
			runExample(t, name)
		})
	}
}

func runExample(t *testing.T, name string) {
	root := findRoot(t)
	srcPath := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
	prog, err := parser.Parse(srcPath)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	c := rack.New()
	code, err := c.Compile(prog)
	if err != nil {
		writeError(t, name, 0, err)
		t.Fatalf("compile error: %v", err)
	}
	code = rack.Format(code)
	outDir := filepath.Join(root, "tests", "machine", "x", "racket")
	os.MkdirAll(outDir, 0755)
	codePath := filepath.Join(outDir, name+".rkt")
	if err := os.WriteFile(codePath, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("racket", codePath)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stdout
	if err := cmd.Run(); err != nil {
		writeError(t, name, 0, err)
		t.Fatalf("run error: %v", err)
	}
	outPath := filepath.Join(outDir, name+".out")
	if err := os.WriteFile(outPath, stdout.Bytes(), 0644); err != nil {
		t.Fatalf("write out: %v", err)
	}
}

func writeError(t *testing.T, name string, line int, err error) {
	outDir := filepath.Join("tests", "machine", "x", "racket")
	os.MkdirAll(outDir, 0755)
	path := filepath.Join(outDir, name+".error")
	msg := []byte(err.Error())
	if line > 0 {
		msg = append([]byte(fmt.Sprintf("line %d:\n", line)), msg...)
	}
	_ = os.WriteFile(path, msg, 0644)
}

func findRoot(t *testing.T) string {
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
