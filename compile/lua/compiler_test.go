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

	luacode "mochi/compile/lua"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_LeetCodeExample1(t *testing.T) {
	t.Skip("disabled in current environment")
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
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
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
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
			cmd := exec.Command("lua", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("lua run error: %v\n%s", err, out)
			}
			got := bytes.TrimSpace(out)
			wantPath := filepath.Join(dir, name+".out")
			if update {
				os.WriteFile(wantPath, got, 0644)
				return
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("failed to read golden: %v", err)
			}
			if !bytes.Equal(bytes.TrimSpace(want), got) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+".out", got, want)
			}
		})
	}
}

func TestLuaCompiler_GoldenOutput(t *testing.T) {
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
			return bytes.TrimSpace(code), nil
		})
	}
}
