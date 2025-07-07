//go:build slow

package zigcode_test

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

	zigcode "mochi/compiler/x/zig"
	"mochi/parser"
	"mochi/types"
)

func TestZigCompiler_ValidPrograms(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig compiler not installed: %v", err)
	}

	srcDir := filepath.Join("..", "..", "..", "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}

	outDir := filepath.Join("..", "..", "..", "tests", "machine", "x", "zig")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { runZigCompile(t, zigc, src, name, outDir) })
	}
}

func runZigCompile(t *testing.T, zigc, srcPath, name, outDir string) {
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
	code, err := zigcode.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, 0, fmt.Errorf("compile error: %w", err))
		return
	}
	zigFile := filepath.Join(outDir, name+".zig")
	if err := os.WriteFile(zigFile, code, 0o644); err != nil {
		t.Fatalf("write zig: %v", err)
	}

	exe := filepath.Join(t.TempDir(), name)
	buildOut, err := exec.Command(zigc, "build-exe", zigFile, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput()
	if err != nil {
		line := parseZigLine(buildOut)
		writeError(outDir, name, code, line, fmt.Errorf("zig build error: %v\n%s", err, buildOut))
		return
	}

	cmd := exec.Command(exe)
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

var zigErrRe = regexp.MustCompile(`:(\d+):`)

func parseZigLine(out []byte) int {
	if m := zigErrRe.FindSubmatch(out); m != nil {
		if n, err := strconv.Atoi(string(m[1])); err == nil {
			return n
		}
	}
	return 0
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
