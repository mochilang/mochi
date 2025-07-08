package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pycode "mochi/compiler/x/python"
	"mochi/parser"
	"mochi/types"
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
	line := 0
	if idx := strings.Index(err.Error(), ":"); idx != -1 {
		fmt.Sscanf(err.Error()[idx+1:], "%d", &line)
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
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "python")
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
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, string(data), errs[0])
				return
			}
			code, err := pycode.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, string(data), err)
				return
			}
			srcFile := filepath.Join(outDir, name+".py")
			os.WriteFile(srcFile, code, 0644)
			cmd := exec.Command("python3", srcFile)
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeError(outDir, name, string(code), fmt.Errorf("run: %v\n%s", err, out))
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
			os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
