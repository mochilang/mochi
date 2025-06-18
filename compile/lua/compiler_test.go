//go:build slow

package luacode_test

import (
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
	dirs := []string{
		"tests/compiler/valid",
		"tests/compiler/lua",
	}
	for _, dir := range dirs {
		golden.Run(t, dir, ".mochi", ".out", func(src string) ([]byte, error) {
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
			dir := t.TempDir()
			file := filepath.Join(dir, "main.lua")
			if err := os.WriteFile(file, code, 0644); err != nil {
				return nil, fmt.Errorf("write error: %w", err)
			}
			cmd := exec.Command("lua", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				return nil, fmt.Errorf("❌ lua run error: %w\n%s", err, out)
			}
			return bytes.TrimSpace(out), nil
		})
	}
}

func TestLuaCompiler_GoldenOutput(t *testing.T) {
	dirs := []string{
		"tests/compiler/valid",
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
