package rustcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rustcode "mochi/compiler/x/rust"
	"mochi/parser"
)

// findRepoRoot walks up directories until go.mod is found.
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

// writeError writes an error file with context around the failing line.
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
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "rust")
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
			code, err := rustcode.New().Compile(prog)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			srcFile := filepath.Join(outDir, name+".rs")
			os.WriteFile(srcFile, code, 0644)
			bin := filepath.Join(outDir, name)
			cmd := exec.Command("rustc", srcFile, "-O", "-o", bin)
			var buf bytes.Buffer
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("compile: %v\n%s", err, buf.String()))
				return
			}
			run := exec.Command(bin)
			out, err := run.CombinedOutput()
			if err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("run: %v\n%s", err, out))
				return
			}
			outPath := filepath.Join(outDir, name+".out")
			os.WriteFile(outPath, bytes.TrimSpace(out), 0644)
		})
	}
}
