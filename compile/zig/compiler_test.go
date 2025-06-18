//go:build slow

package zigcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	zigcode "mochi/compile/zig"
	"mochi/parser"
	"mochi/types"
)

func TestZigCompiler_LeetCode1to3(t *testing.T) {
	out, err := runExample(t, 1)
	if err != nil {
		t.Fatalf("run error: %v", err)
	}
	got := strings.TrimSpace(string(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}

	_, err = runExample(t, 2)
	if err != nil {
		t.Fatalf("run error: %v", err)
	}

	_, err = runExample(t, 3)
	if err != nil {
		t.Fatalf("run error: %v", err)
	}
}

func runExample(t *testing.T, id int) ([]byte, error) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig compiler not installed: %v", err)
		return nil, nil
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		return nil, fmt.Errorf("glob error: %w", err)
	}
	if len(files) == 0 {
		return nil, fmt.Errorf("no examples for %d", id)
	}
	var buf bytes.Buffer
	for _, src := range files {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		c := zigcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		tmp := t.TempDir()
		file := filepath.Join(tmp, "main.zig")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(tmp, "main")
		if out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("zig build error: %w\n%s", err, out)
		}
		out, err := exec.Command(exe).CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("run error: %w\n%s", err, out)
		}
		buf.Write(out)
	}
	return buf.Bytes(), nil
}
