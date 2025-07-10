//go:build slow

package gocode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	gocode "mochi/compiler/x/go"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestGoCompiler_ValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}

	srcDir := filepath.Join("..", "..", "..", "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}

	outDir := filepath.Join("..", "..", "..", "tests", "machine", "x", "go")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { runGoCompile(t, src, name, outDir) })
	}
}

func runGoCompile(t *testing.T, srcPath, name, outDir string) {
	data, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeError(outDir, name, data, 0, fmt.Errorf("parse error: %w", err))
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeError(outDir, name, data, 0, fmt.Errorf("type error: %v", errs[0]))
		return
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, 0, fmt.Errorf("compile error: %w", err))
		return
	}
	goFile := filepath.Join(outDir, name+".go")
	if err := os.WriteFile(goFile, code, 0o644); err != nil {
		t.Fatalf("write go: %v", err)
	}

	cmd := exec.Command("go", "run", goFile)
	if inData, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(inData)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, code, 0, fmt.Errorf("run error: %v\n%s", err, out))
		return
	}
	got := bytes.TrimSpace(out)
	wantPath := filepath.Join(filepath.Dir(srcPath), name+".out")
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
	fmt.Fprintf(&buf, "Error on line %d: %v\n", line, err)
	for i := start; i < end; i++ {
		fmt.Fprintf(&buf, "%d: %s\n", i+1, lines[i])
	}
	_ = os.WriteFile(path, buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	os.Exit(code)
}
