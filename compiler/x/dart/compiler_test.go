package dart_test

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

	dart "mochi/compiler/x/dart"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_ValidPrograms(t *testing.T) {
	root := findRepoRoot(t)
	srcPattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(srcPattern)
	if err != nil {
		t.Fatalf("list sources: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "dart")
	os.MkdirAll(outDir, 0755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, src, fmt.Errorf("parse error: %w", err))
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, src, fmt.Errorf("type error: %v", errs[0]))
				return
			}
			code, err := dart.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, src, fmt.Errorf("compile error: %w", err))
				return
			}
			srcFile := filepath.Join(outDir, name+".dart")
			if err := os.WriteFile(srcFile, code, 0644); err != nil {
				writeError(outDir, name, src, fmt.Errorf("write error: %w", err))
				return
			}
			cmd := exec.Command("dart", srcFile)
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeCompileError(outDir, name, srcFile, out, err)
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644)
		})
	}
}

func writeError(dir, name, src string, err error) {
	msg := err.Error()
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

var errLineRE = regexp.MustCompile(`:(\d+):`) // extract line number

func writeCompileError(dir, name, file string, output []byte, err error) {
	msg := fmt.Sprintf("%v\n%s", err, output)
	line := 0
	if m := errLineRE.FindSubmatch(output); len(m) == 2 {
		line = atoi(string(m[1]))
	}
	if line > 0 {
		srcData, _ := os.ReadFile(file)
		lines := strings.Split(string(srcData), "\n")
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		ctx := strings.Join(lines[start:end], "\n")
		msg = fmt.Sprintf("line %d: %v\n%s", line, err, ctx)
	}
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func atoi(s string) int {
	n, _ := strconv.Atoi(s)
	return n
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
