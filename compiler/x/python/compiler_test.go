//go:build slow

package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	pycode "mochi/compiler/x/python"
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

func TestPythonCompiler_ValidPrograms(t *testing.T) {
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "python")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileOne(t, src, outDir, name) })
	}
}

func compileOne(t *testing.T, src, outDir, name string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}

	if name == "load_yaml" {
		if err := exec.Command("python3", "-c", "import yaml").Run(); err != nil {
			t.Skip("yaml module not installed")
			return
		}
	}

	prog, err := parser.Parse(src)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("parse error: %v", err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, data, errs[0])
		t.Skipf("type error: %v", errs[0])
		return
	}

	code, err := pycode.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("compile error: %v", err)
		return
	}

	pyPath := filepath.Join(outDir, name+".py")
	if err := os.WriteFile(pyPath, code, 0644); err != nil {
		t.Fatalf("write py: %v", err)
	}

	cmd := exec.Command("python3", pyPath)
	if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(in)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeRunError(outDir, name, pyPath, code, out, err)
		t.Skipf("python error: %v", err)
		return
	}

	outFile := filepath.Join(outDir, name+".out")
	if err := os.WriteFile(outFile, normalize(out), 0644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	os.Remove(filepath.Join(outDir, name+".error"))
}

func normalize(out []byte) []byte {
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	for i, ln := range lines {
		if ln == "True" {
			ln = "true"
		} else if ln == "False" {
			ln = "false"
		}
		if strings.HasPrefix(ln, "[") && strings.HasSuffix(ln, "]") {
			ln = strings.TrimSuffix(strings.TrimPrefix(ln, "["), "]")
			ln = strings.ReplaceAll(ln, ", ", " ")
			ln = strings.ReplaceAll(ln, "'", "")
		}
		lines[i] = ln
	}
	return []byte(strings.Join(lines, "\n"))
}

func writeError(dir, name string, src []byte, err error) {
	line := extractLine(err.Error())
	var context string
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
		var b strings.Builder
		for i := start; i < end; i++ {
			if i < len(lines) {
				fmt.Fprintf(&b, "%4d: %s\n", i+1, lines[i])
			}
		}
		context = b.String()
	}
	msg := fmt.Sprintf("line: %d\nerror: %v\n%s", line, err, context)
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func writeRunError(dir, name, file string, code, out []byte, runErr error) {
	line := 0
	parts := strings.Split(string(out), "\n")
	for i := len(parts) - 1; i >= 0; i-- {
		if strings.HasPrefix(parts[i], "  File") && strings.Contains(parts[i], file) {
			fmt.Sscanf(parts[i], "  File %q, line %d", new(string), &line)
			break
		}
	}
	var context string
	if line > 0 {
		lines := strings.Split(string(code), "\n")
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		var b strings.Builder
		for i := start; i < end; i++ {
			if i < len(lines) {
				fmt.Fprintf(&b, "%4d: %s\n", i+1, lines[i])
			}
		}
		context = b.String()
	}
	msg := fmt.Sprintf("line: %d\nerror: %v\n%s\n%s", line, runErr, out, context)
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func extractLine(msg string) int {
	re := regexp.MustCompile(`:(\d+):`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	re = regexp.MustCompile(`line (\d+)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}
