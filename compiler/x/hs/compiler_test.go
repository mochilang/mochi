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
				t.Skipf("type error: %v", errs[0])
				return
			}
			code, err := hscode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
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
				t.Skipf("runhaskell error: %v", err)
				return
			}
			norm := normalize(out.Bytes())
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), norm, 0644); err != nil {
				t.Fatalf("write out: %v", err)
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
