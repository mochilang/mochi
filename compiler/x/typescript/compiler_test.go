//go:build slow

package typescriptcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	typescriptcode "mochi/compiler/x/typescript"
	"mochi/parser"
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

func writeError(dir, name, src string, err error) {
	lines := strings.Split(src, "\n")
	msg := err.Error()
	line := 0
	if idx := strings.Index(msg, ":"); idx != -1 {
		fmt.Sscanf(msg[idx+1:], "%d", &line)
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	ctx := strings.Join(lines[start:end], "\n")
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(fmt.Sprintf("line %d: %v\n%s", line, err, ctx)), 0644)
}

func TestCompilePrograms(t *testing.T) {
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "typescript")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatal(err)
			}
			prog, err := parser.ParseString(string(data))
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			code, err := typescriptcode.New().Compile(prog)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			srcFile := filepath.Join(outDir, name+".ts")
			os.WriteFile(srcFile, code, 0644)
			cmd := exec.Command("deno", "run", "--quiet", srcFile)
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("run: %v\n%s", err, out))
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
		})
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "typescript")
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
	buf.WriteString("# Machine-generated TypeScript Programs\n\n")
	buf.WriteString("This directory contains TypeScript code compiled from Mochi programs in `tests/vm/valid` using the experimental compiler.\n\n")
	fmt.Fprintf(&buf, "## Progress\n\nCompiled: %d/%d programs\n\n", compiled, total)
	buf.WriteString("## Checklist\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
