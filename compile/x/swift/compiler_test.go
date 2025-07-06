//go:build slow

package swiftcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	swiftcode "mochi/compile/x/swift"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_SubsetPrograms(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/swift", ".mochi", ".out", func(src string) ([]byte, error) {
		swiftOut, err := compileAndRunSwift(src)
		if err != nil {
			return nil, err
		}
		vmOut, err := runMochiVM(src)
		if err != nil {
			return nil, fmt.Errorf("vm error: %w", err)
		}
		if !bytes.Equal(bytes.TrimSpace(swiftOut), bytes.TrimSpace(vmOut)) {
			return nil, fmt.Errorf("runtime mismatch\n-- swift --\n%s\n\n-- vm --\n%s\n", swiftOut, vmOut)
		}
		return bytes.TrimSpace(swiftOut), nil
	})
}

func TestSwiftCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/swift", ".mochi", ".swift.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := swiftcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestSwiftCompiler_LeetCodeExample1(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	runLeetExample(t, 1, "0\n1")
}

func TestSwiftCompiler_LeetCodeExample2(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	runLeetExample(t, 2, "")
}

func TestSwiftCompiler_LeetCodeExample3(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	runLeetExample(t, 3, "")
}

// TestSwiftCompiler_LeetCodeRange compiles and runs the first ten LeetCode
// examples using the Swift backend. Only the first problem prints output, so
// we verify its expected result and ensure the others run without errors.
func TestSwiftCompiler_LeetCodeRange(t *testing.T) {
	if err := swiftcode.EnsureSwift(); err != nil {
		t.Skipf("swift not installed: %v", err)
	}
	for i := 1; i <= 10; i++ {
		want := ""
		if i == 1 {
			want = "0\n1"
		}
		runLeetExample(t, i, want)
	}
}

func runLeetExample(t *testing.T, id int, want string) {
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			out := runSwiftProgram(t, f)
			if want != "" {
				got := string(out)
				if got != want {
					t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
				}
			}
		})
	}
}

func runSwiftProgram(t *testing.T, src string) []byte {
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := swiftcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("swiftc", file, "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("swiftc error: %v\n%s", err, out)
	}
	cmd := exec.Command(exe)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("swift run error: %v\n%s", err, out)
	}
	return bytes.TrimSpace(out)
}
