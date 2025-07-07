package hscode_test

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

	hscode "mochi/compiler/x/hs"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

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

func TestHSCompiler_ValidPrograms(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "hs")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := hscode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			hsPath := filepath.Join(outDir, name+".hs")
			if err := os.WriteFile(hsPath, code, 0644); err != nil {
				t.Fatalf("write hs: %v", err)
			}
			cmd := exec.Command("runhaskell", hsPath)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			var out bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &out
			err = cmd.Run()
			if err != nil {
				writeError(t, outDir, name, hsPath, code, out.Bytes())
				t.Fatalf("runhaskell error: %v", err)
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), out.Bytes(), 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("vm compile error: %v", err)
			}
			var vmOut bytes.Buffer
			if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				m := vm.NewWithIO(p, bytes.NewReader(in), &vmOut)
				if err := m.Run(); err != nil {
					t.Fatalf("vm run error: %v", err)
				}
			} else {
				m := vm.New(p, &vmOut)
				if err := m.Run(); err != nil {
					t.Fatalf("vm run error: %v", err)
				}
			}
			vmRes := strings.TrimSpace(vmOut.String())
			hsRes := strings.TrimSpace(out.String())
			if vmRes != hsRes {
				t.Fatalf("output mismatch\n-- hs --\n%s\n-- vm --\n%s", hsRes, vmRes)
			}
		})
	}
}

func writeError(t *testing.T, dir, name, file string, code, out []byte) {
	t.Helper()
	errPath := filepath.Join(dir, name+".error")
	var buf bytes.Buffer
	buf.Write(out)
	re := regexp.MustCompile(regexp.QuoteMeta(file) + `:(\d+)`)
	if m := re.FindSubmatch(out); len(m) == 2 {
		line, _ := strconv.Atoi(string(m[1]))
		lines := strings.Split(string(code), "\n")
		start := line - 3
		if start < 0 {
			start = 0
		}
		end := line + 2
		if end > len(lines) {
			end = len(lines)
		}
		fmt.Fprintf(&buf, "\n\nContext around line %d:\n", line)
		for i := start; i < end; i++ {
			fmt.Fprintf(&buf, "%5d| %s\n", i+1, lines[i])
		}
	}
	_ = os.WriteFile(errPath, buf.Bytes(), 0644)
}
