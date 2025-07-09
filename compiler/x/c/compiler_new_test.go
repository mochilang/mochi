package ccode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ccode "mochi/compiler/x/c"
	"mochi/parser"
	"mochi/types"
)

func TestCCompiler_ValidPrograms(t *testing.T) {
	cc, err := exec.LookPath("gcc")
	if err != nil {
		t.Skip("gcc not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "c")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		t.Run(name, func(t *testing.T) {
			runCCompile(t, cc, srcPath, outDir, name)
		})
	}
}

func runCCompile(t *testing.T, cc, srcPath, outDir, name string) {
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	srcDir := filepath.Dir(srcPath)
	srcData, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read source: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeError(outDir, name, srcData, 0, fmt.Errorf("parse error: %w", err))
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, srcData, 0, fmt.Errorf("type error: %v", errs[0]))
		return
	}
	code, err := ccode.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, srcData, 0, fmt.Errorf("compile error: %w", err))
		return
	}
	cFile := filepath.Join(outDir, name+".c")
	if err := os.WriteFile(cFile, code, 0o644); err != nil {
		t.Fatalf("write c: %v", err)
	}
	tmp := t.TempDir()
	bin := filepath.Join(tmp, name)
	if out, err := exec.Command(cc, cFile, "-o", bin).CombinedOutput(); err != nil {
		writeError(outDir, name, code, 0, fmt.Errorf("cc error: %w\n%s", err, out))
		return
	}
	cmd := exec.Command(bin)
	if inData, err := os.ReadFile(filepath.Join(srcDir, name+".in")); err == nil {
		cmd.Stdin = bytes.NewReader(inData)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, code, 0, fmt.Errorf("run error: %w\n%s", err, out))
		return
	}
	got := bytes.TrimSpace(out)
	wantPath := filepath.Join(srcDir, name+".out")
	if want, err := os.ReadFile(wantPath); err == nil {
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			writeError(outDir, name, code, 0, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
			return
		}
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeError(dir, name string, src []byte, line int, err error) {
	path := filepath.Join(dir, name+".error")
	lines := strings.Split(string(src), "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "line: %d\nerror: %v\n", line, err)
	for i := start; i < end; i++ {
		fmt.Fprintf(&buf, " %3d: %s\n", i+1, lines[i])
	}
	_ = os.WriteFile(path, buf.Bytes(), 0o644)
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
