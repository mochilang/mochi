//go:build slow

package cobol_test

import (
	"bytes"
	"fmt"
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

func TestCobolCompiler_Programs(t *testing.T) {
	cobc := ensureCobc(t)
	root := repoRoot()
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "cobol")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, _ := os.ReadFile(src)
			errPath := filepath.Join(outDir, name+".error")
			os.Remove(errPath)
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, string(data), errs[0])
				return
			}
			code, err := cobol.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			srcFile := filepath.Join(outDir, name+".cob")
			os.WriteFile(srcFile, code, 0644)
			tmp := t.TempDir()
			bin := filepath.Join(tmp, name)
			if out, err := exec.Command(cobc, "-free", "-x", "-o", bin, srcFile).CombinedOutput(); err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("cobc: %v\n%s", err, out))
				return
			}
			out, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("run: %v\n%s", err, out))
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
		})
	}
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
		ctx = strings.Join(lines[start:end], "\n")
	}
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg+"\n"+ctx), 0644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "cobol")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi to COBOL Machine Outputs\n\n")
	buf.WriteString("This directory contains COBOL code generated from the programs in `tests/vm/valid`.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
