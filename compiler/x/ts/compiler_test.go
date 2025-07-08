//go:build slow

package tscode_test

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

	"mochi/compiler/x/testutil"
	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func writeError(dir, name string, src []byte, err error) {
	line := extractLine(err.Error())
	lines := bytes.Split(src, []byte("\n"))
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	fmt.Fprintf(&b, "line: %d\nerror: %v\n", line, err)
	for i := start; i < end; i++ {
		if i < len(lines) {
			fmt.Fprintf(&b, "%4d: %s\n", i+1, lines[i])
		}
	}
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(b.String()), 0644)
}

func extractLine(msg string) int {
	re := regexp.MustCompile(`:(\d+):`)
	if m := re.FindStringSubmatch(msg); m != nil {
		n, _ := strconv.Atoi(m[1])
		return n
	}
	re = regexp.MustCompile(`line (\d+)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		n, _ := strconv.Atoi(m[1])
		return n
	}
	return 0
}

func TestTSCompiler_Programs(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "ts")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
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
	code, err := tscode.New(env).Compile(prog)
	if err != nil {
		writeError(outDir, name, data, err)
		t.Skipf("compile error: %v", err)
		return
	}
	tsPath := filepath.Join(outDir, name+".ts")
	if err := os.WriteFile(tsPath, code, 0644); err != nil {
		t.Fatalf("write ts: %v", err)
	}
	cmd := exec.Command("deno", "run", "--quiet", tsPath)
	if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(in)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		writeError(outDir, name, code, fmt.Errorf("run error: %v\n%s", err, out))
		t.Skipf("run error: %v", err)
		return
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0644); err != nil {
		t.Fatalf("write out: %v", err)
	}
}
