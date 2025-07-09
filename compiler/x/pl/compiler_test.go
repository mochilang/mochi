//go:build slow

package pl_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pl "mochi/compiler/x/pl"
	"mochi/parser"
)

func TestPrologCompiler(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "pl")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileAndRun(t, src, outDir, name) })
	}
}

func compileAndRun(t *testing.T, src, outDir, name string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.ParseString(string(data))
	if err != nil {
		writeError(outDir, name, string(data), err)
		t.Fatalf("parse error: %v", err)
	}
	code, err := pl.New().Compile(prog)
	if err != nil {
		writeError(outDir, name, string(data), err)
		t.Fatalf("compile error: %v", err)
	}
	codePath := filepath.Join(outDir, name+".pl")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	cmd := exec.Command("swipl", "-q", codePath)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeError(outDir, name, string(data), fmt.Errorf("run: %v\n%s", err, buf.String()))
		t.Fatalf("run error: %v", err)
	}
	os.WriteFile(filepath.Join(outDir, name+".out"), buf.Bytes(), 0o644)
}

func writeError(dir, name, src string, err error) {
	lines := strings.Split(src, "\n")
	msg := err.Error()
	ln := 0
	if idx := strings.Index(msg, "line "); idx != -1 {
		fmt.Sscanf(msg[idx:], "line %d", &ln)
	}
	var ctx string
	if ln > 0 {
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			ctx += lines[i] + "\n"
		}
	}
	errPath := filepath.Join(dir, name+".error")
	os.WriteFile(errPath, []byte(msg+"\n"+ctx), 0o644)
}

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
