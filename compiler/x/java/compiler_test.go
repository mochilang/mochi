//go:build slow

package javacode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	javacode "mochi/compiler/x/java"
	"mochi/parser"
)

func context(src string, line int) string {
	data, _ := os.ReadFile(src)
	lines := strings.Split(string(data), "\n")
	if line < 1 {
		line = 1
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var out []string
	for i := start; i < end; i++ {
		out = append(out, fmt.Sprintf("%3d| %s", i+1, lines[i]))
	}
	return strings.Join(out, "\n")
}

func classNameFromVar(s string) string {
	if s == "" {
		return ""
	}
	parts := strings.FieldsFunc(s, func(r rune) bool {
		return r == '_' || r == '-' || r == ' '
	})
	for i, p := range parts {
		if p == "" {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

func TestCompileValidPrograms(t *testing.T) {
	root := filepath.Join("..", "..", "..")
	dir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "java")
	os.MkdirAll(outDir, 0755)
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		className := classNameFromVar(name)
		if className == "" {
			className = "Main"
		}
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				writeError(outDir, name, f, 0, fmt.Sprintf("parse error: %v", err))
				return
			}
			code, err := javacode.New().Compile(prog)
			if err != nil {
				writeError(outDir, name, f, 0, fmt.Sprintf("compile error: %v", err))
				return
			}
			srcFile := filepath.Join(outDir, name+".java")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			tmp := t.TempDir()
			mainFile := filepath.Join(tmp, className+".java")
			os.WriteFile(mainFile, code, 0644)
			if out, err := exec.Command("javac", mainFile).CombinedOutput(); err != nil {
				writeError(outDir, name, f, 0, fmt.Sprintf("javac error: %v\n%s", err, out))
				return
			}
			cmd := exec.Command("java", "-cp", tmp, className)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeError(outDir, name, f, 0, fmt.Sprintf("run error: %v\n%s", err, buf.Bytes()))
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(buf.Bytes()), 0644)
			os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeError(outDir, name, src string, line int, msg string) {
	ctx := context(src, line)
	full := fmt.Sprintf("line %d: %s\n\n%s", line, msg, ctx)
	os.WriteFile(filepath.Join(outDir, name+".error"), []byte(full), 0644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	os.Exit(code)
}


func repoRoot() string {
	dir, _ := os.Getwd()
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
	return dir
}
