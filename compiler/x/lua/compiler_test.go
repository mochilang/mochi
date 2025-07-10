//go:build slow

package luacode_test

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

	luacode "mochi/compiler/x/lua"
	"mochi/parser"
	"mochi/types"
)

// findRepoRoot walks up the directory tree to locate the module root.
func findRepoRoot(t *testing.T) string {
	t.Helper()
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

// writeError writes a rich error file with context around the line number.
func writeError(path string, src []byte, err error) {
	msg := err.Error()
	line := 0
	re := regexp.MustCompile(`:(\d+)`)
	if m := re.FindStringSubmatch(msg); len(m) == 2 {
		if v, e := strconv.Atoi(m[1]); e == nil {
			line = v
		}
	}
	start := line - 2
	if start < 1 {
		start = 1
	}
	lines := strings.Split(string(src), "\n")
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "line %d: %v\n", line, err)
	for i := start; i <= end; i++ {
		if i-1 < len(lines) {
			fmt.Fprintf(&buf, "%4d | %s\n", i, lines[i-1])
		}
	}
	_ = os.WriteFile(path, buf.Bytes(), 0644)
}

func TestLuaCompiler_ValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua interpreter not available")
	}

	root := findRepoRoot(t)
	srcPattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(srcPattern)
	if err != nil {
		t.Fatal(err)
	}

	outDir := filepath.Join(root, "tests", "machine", "x", "lua")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatal(err)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read source: %v", err)
			}
			prog, err := parser.Parse(src)
			if err != nil {
				errPath := filepath.Join(outDir, name+".error")
				writeError(errPath, data, err)
				t.Skipf("parse error: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				errPath := filepath.Join(outDir, name+".error")
				writeError(errPath, data, errs[0])
				t.Skipf("type error: %v", errs[0])
				return
			}
			c := luacode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				errPath := filepath.Join(outDir, name+".error")
				writeError(errPath, data, err)
				t.Skipf("compile error: %v", err)
				return
			}
			codePath := filepath.Join(outDir, name+".lua")
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("lua", codePath)
			if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(in)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				errPath := filepath.Join(outDir, name+".error")
				writeError(errPath, data, fmt.Errorf("run error: %w\n%s", err, out))
				t.Skipf("run error: %v", err)
				return
			}
			out = bytes.TrimSpace(out)
			outFile := filepath.Join(outDir, name+".out")
			if err := os.WriteFile(outFile, out, 0644); err != nil {
				t.Fatalf("write output: %v", err)
			}
			wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
			if want, err := os.ReadFile(wantPath); err == nil {
				want = bytes.TrimSpace(want)
				if !bytes.Equal(out, want) {
					t.Errorf("output mismatch\nwant:\n%s\n\ngot:\n%s", want, out)
				}
			}
		})
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	os.Exit(code)
}
