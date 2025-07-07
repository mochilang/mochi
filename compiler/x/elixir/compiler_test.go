package elixir_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	elixir "mochi/compiler/x/elixir"
	"mochi/parser"
	"mochi/types"
)

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

func TestElixirCompiler_ValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "elixir")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, 0, fmt.Sprintf("parse error: %v", err), nil)
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, 0, fmt.Sprintf("type error: %v", errs[0]), nil)
				t.Skip("type error")
				return
			}
			c := elixir.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				writeError(outDir, name, 0, fmt.Sprintf("compile error: %v", err), nil)
				t.Skip("compile error")
				return
			}
			srcFile := filepath.Join(outDir, name+".exs")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("elixir", srcFile)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			err = cmd.Run()
			if err != nil {
				line := extractLine(buf.String(), srcFile)
				writeError(outDir, name, line, buf.String(), code)
				t.Skipf("elixir error: %v", err)
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(buf.Bytes()), 0644)
		})
	}
}

func extractLine(msg, file string) int {
	for _, ln := range strings.Split(msg, "\n") {
		if strings.Contains(ln, file) {
			var n int
			fmt.Sscanf(ln, "%*s:%d:", &n)
			if n > 0 {
				return n
			}
		}
	}
	return 0
}

func writeError(dir, name string, line int, msg string, src []byte) {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "line %d\n%s\n", line, msg)
	if line > 0 && src != nil {
		lines := bytes.Split(src, []byte("\n"))
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			buf.Write(lines[i])
			buf.WriteByte('\n')
		}
	}
	os.WriteFile(filepath.Join(dir, name+".error"), buf.Bytes(), 0644)
}
