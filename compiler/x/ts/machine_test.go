//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func TestGenerateMachineOutput(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot3(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	outDir := filepath.Join(root, "tests", "machine", "x", "ts")
	_ = os.MkdirAll(outDir, 0755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, err)
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, errs[0])
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := tscode.New(env, filepath.Dir(src)).Compile(prog)
			if err != nil {
				writeError(outDir, name, err)
				t.Fatalf("compile error: %v", err)
			}
			codePath := filepath.Join(outDir, name+".ts")
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", codePath)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			cmd.Dir = filepath.Join(filepath.Dir(src), "..")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeError(outDir, name, fmt.Errorf("run error: %w\n%s", err, out))
				t.Fatalf("run error: %v", err)
			}
			out = bytes.TrimSpace(out)
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeError(dir, name string, err error) {
	path := filepath.Join(dir, name+".error")
	_ = os.WriteFile(path, []byte(err.Error()), 0644)
}

func findRepoRoot3(t *testing.T) string {
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot3(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "ts")
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
