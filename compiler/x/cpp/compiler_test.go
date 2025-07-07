package cpp_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cpp "mochi/compiler/x/cpp"
	"mochi/parser"
)

// TestCompilePrograms compiles each Mochi sample to C++ and runs it.
// The generated source and outputs are written under tests/machine/x/cpp.
func TestCompilePrograms(t *testing.T) {
	files, err := filepath.Glob(filepath.Join("..", "..", "..", "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	outDir := filepath.Join("..", "..", "..", "tests", "machine", "x", "cpp")
	os.MkdirAll(outDir, 0755)

	for _, f := range files {
		name := filepath.Base(f)
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(f)
			if err != nil {
				t.Fatal(err)
			}
			prog, err := parser.ParseString(string(data))
			if err != nil {
				writeError(outDir, name, 0, fmt.Sprintf("parse error: %v", err), data)
				return
			}
			c := cpp.New()
			code, err := c.Compile(prog)
			if err != nil {
				writeError(outDir, name, 0, fmt.Sprintf("compile error: %v", err), data)
				return
			}
			srcFile := filepath.Join(outDir, name+".cpp")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				t.Fatal(err)
			}
			bin := filepath.Join(outDir, name+".bin")
			cmd := exec.Command("g++", srcFile, "-std=c++17", "-o", bin)
			if out, err := cmd.CombinedOutput(); err != nil {
				parseLine := extractLine(string(out))
				writeError(outDir, name, parseLine, string(out), code)
				return
			}
			run := exec.Command(bin)
			var buf bytes.Buffer
			run.Stdout = &buf
			if err := run.Run(); err != nil {
				writeError(outDir, name, 0, fmt.Sprintf("run error: %v", err), code)
				return
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), buf.Bytes(), 0644); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func extractLine(msg string) int {
	for _, line := range strings.Split(msg, "\n") {
		if i := strings.Index(line, ":"); i != -1 {
			var n int
			fmt.Sscanf(line[i+1:], "%d", &n)
			if n > 0 {
				return n
			}
		}
	}
	return 0
}

func writeError(dir, name string, line int, msg string, src []byte) {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "line %d: %s\n", line, msg)
	if line > 0 {
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
			fmt.Fprintf(&buf, "%3d | %s\n", i+1, lines[i])
		}
	}
	os.WriteFile(filepath.Join(dir, name+".error"), buf.Bytes(), 0644)
}
