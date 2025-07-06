//go:build slow

package luacode_test

import (
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"bytes"
	"fmt"

	luacode "mochi/compile/x/lua"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestLuaCompiler_LeetCodeExample1(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	src := filepath.Join("..", "..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := luacode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.lua")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("lua", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("lua run error: %v\n%s", err, out)
	}
	got := strings.ReplaceAll(string(out), "\r\n", "\n")
	if strings.TrimSpace(got) != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestLuaCompiler_SubsetPrograms(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	skip := map[string]bool{
		"list_prepend.mochi":        true,
		"local_recursion.mochi":     true,
		"slice_remove.mochi":        true,
		"typed_list_negative.mochi": true,
		"union_inorder.mochi":       true,
	}
	dir := "tests/compiler/lua"
	pattern := filepath.Join(dir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	updateFlag := flag.Lookup("update")
	update := updateFlag != nil && updateFlag.Value.String() == "true"
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if skip[filepath.Base(src)] {
			continue
		}
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := luacode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.lua")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}

			var input []byte
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				input = data
			}

			// run compiled Lua code
			cmd := exec.Command("lua", file)
			if len(input) > 0 {
				cmd.Stdin = bytes.NewReader(input)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("lua run error: %v\n%s", err, out)
			}
			got := bytes.TrimSpace(out)

			// run original code with the VM for expected output
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("vm compile error: %v", err)
			}
			var vmOut bytes.Buffer
			m := vm.NewWithIO(p, bytes.NewReader(input), &vmOut)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					t.Fatalf("vm run error:\n%s", ve.Format(p))
				}
				t.Fatalf("vm run error: %v", err)
			}
			want := bytes.TrimSpace(vmOut.Bytes())

			wantPath := filepath.Join(dir, name+".out")
			if update {
				os.WriteFile(wantPath, want, 0644)
			}

			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch for %s\n\n--- VM ---\n%s\n\n--- Lua ---\n%s\n", name, want, got)
			}
		})
	}
}

func TestLuaCompiler_GoldenOutput(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	dirs := []string{
		"tests/compiler/lua",
	}
	for _, dir := range dirs {
		golden.Run(t, dir, ".mochi", ".lua.out", func(src string) ([]byte, error) {
			prog, err := parser.Parse(src)
			if err != nil {
				return nil, fmt.Errorf("❌ parse error: %w", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				return nil, fmt.Errorf("❌ type error: %v", errs[0])
			}
			c := luacode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				return nil, fmt.Errorf("❌ compile error: %w", err)
			}

			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.lua")
			if err := os.WriteFile(file, code, 0644); err != nil {
				return nil, fmt.Errorf("write error: %w", err)
			}

			var input []byte
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				input = data
			}

			cmd := exec.Command("lua", file)
			if len(input) > 0 {
				cmd.Stdin = bytes.NewReader(input)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				return nil, fmt.Errorf("lua run error: %w\n%s", err, out)
			}
			got := bytes.TrimSpace(out)

			p, err := vm.Compile(prog, env)
			if err != nil {
				return nil, fmt.Errorf("vm compile error: %w", err)
			}
			var vmOut bytes.Buffer
			m := vm.NewWithIO(p, bytes.NewReader(input), &vmOut)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
				}
				return nil, fmt.Errorf("vm run error: %v", err)
			}
			want := bytes.TrimSpace(vmOut.Bytes())

			updateFlag := flag.Lookup("update")
			update := updateFlag != nil && updateFlag.Value.String() == "true"
			wantPath := filepath.Join(dir, strings.TrimSuffix(filepath.Base(src), ".mochi")+".out")
			if update {
				os.WriteFile(wantPath, want, 0644)
			}

			if !bytes.Equal(got, want) {
				return nil, fmt.Errorf("output mismatch for %s\n\n--- VM ---\n%s\n\n--- Lua ---\n%s\n", filepath.Base(src), want, got)
			}

			return bytes.TrimSpace(code), nil
		})
	}
}

func TestLuaCompiler_LeetCodeExamples(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	runLeetCode(t, 1, "0\n1")
	for i := 2; i <= 10; i++ {
		runLeetCode(t, i, "")
	}
}

func runLeetCode(t *testing.T, id int, want string) {
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := luacode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.lua")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("lua", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("lua run error: %v\n%s", err, out)
			}
			if want != "" {
				got := strings.TrimSpace(string(out))
				if got != want {
					t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
				}
			}
		})
	}
}
