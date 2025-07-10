//go:build slow

package racket_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rack "mochi/compiler/x/racket"
	"mochi/parser"
)

func TestRacketCompiler(t *testing.T) {
	if err := rack.EnsureRacket(); err != nil {
		t.Skipf("racket not available: %v", err)
	}
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "racket")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			compileAndRun(t, src, outDir, name)
		})
	}
}

func compileAndRun(t *testing.T, src, outDir, name string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read error: %v", err)
	}
	errPath := filepath.Join(outDir, name+".error")
	os.Remove(errPath)
	prog, err := parser.ParseString(string(data))
	if err != nil {
		writeError(t, outDir, name, string(data), err)
		return
	}
	code, err := rack.New().Compile(prog)
	if err != nil {
		writeError(t, outDir, name, string(data), err)
		return
	}
	code = rack.Format(code)
	codePath := filepath.Join(outDir, name+".rkt")
	os.WriteFile(codePath, code, 0644)
	cmd := exec.Command("racket", codePath)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeError(t, outDir, name, string(data), fmt.Errorf("run: %v\n%s", err, buf.String()))
		return
	}
	os.WriteFile(filepath.Join(outDir, name+".out"), buf.Bytes(), 0644)
}

func writeError(t *testing.T, dir, name, src string, err error) {
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
	os.WriteFile(errPath, []byte(msg+"\n"+ctx), 0644)
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

func TestMain(m *testing.M) {
	code := m.Run()
	os.Exit(code)
}
