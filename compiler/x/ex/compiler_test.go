//go:build slow

package excode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

func TestElixirCompiler_ValidPrograms(t *testing.T) {
	if err := excode.EnsureElixir(); err != nil {
		t.Skipf("elixir not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "..", "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	os.MkdirAll(filepath.Join("..", "..", "..", "tests", "machine", "x", "ex"), 0755)
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Errorf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Errorf("type error: %v", errs[0])
				return
			}
			outDir := filepath.Join("..", "..", "..", "tests", "machine", "x", "ex")
			code, err := excode.New(env).Compile(prog)
			if err != nil {
				writeError(t, outDir, name, []byte(err.Error()), nil)
				t.Logf("compile error: %v", err)
				return
			}
			srcPath := filepath.Join(outDir, name+".exs")
			if err := os.WriteFile(srcPath, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("elixir", srcPath)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			err = cmd.Run()
			if err != nil {
				writeError(t, outDir, name, buf.Bytes(), code)
				t.Logf("elixir error: %v", err)
				return
			}
			outPath := filepath.Join(outDir, name+".out")
			if err := os.WriteFile(outPath, bytes.TrimSpace(buf.Bytes()), 0644); err != nil {
				t.Fatalf("write out error: %v", err)
			}
		})
	}
}

func writeError(t *testing.T, dir, name string, out, src []byte) {
	errFile := filepath.Join(dir, name+".error")
	line := 0
	msg := string(out)
	re := regexp.MustCompile(`(?m):(\d+):`)
	if m := re.FindStringSubmatch(msg); len(m) == 2 {
		fmt.Sscanf(m[1], "%d", &line)
	}
	context := []byte{}
	if line > 0 {
		lines := bytes.Split(src, []byte("\n"))
		start := line - 3
		if start < 0 {
			start = 0
		}
		end := line + 2
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			context = append(context, lines[i]...)
			context = append(context, '\n')
		}
	}
	os.WriteFile(errFile, []byte(fmt.Sprintf("line %d\n%s\n%s", line, msg, context)), 0644)
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

func TestMain(m *testing.M) {
	// Ensure generated headers are deterministic for golden tests
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "ex")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	var remaining []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		} else {
			remaining = append(remaining, name)
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Elixir Machine Output\n\n")
	buf.WriteString("This directory contains Elixir source code generated from Mochi programs and the corresponding outputs.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if len(remaining) > 0 {
		buf.WriteString("\n### Remaining tasks\n")
		for _, r := range remaining {
			buf.WriteString("- [ ] " + r + "\n")
		}
	}
	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
