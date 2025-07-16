//go:build slow

package luacode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdate() bool {
	v := os.Getenv("UPDATE")
	return v == "1" || strings.ToLower(v) == "true"
}

func TestLuaCompiler_VMValid_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	goldenDir := filepath.Join(root, "tests", "machine", "x", "lua")

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(filepath.Join(goldenDir, name+".error"), []byte("parse: "+err.Error()), 0o644)
				}
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				if shouldUpdate() {
					_ = os.WriteFile(filepath.Join(goldenDir, name+".error"), []byte("type: "+errs[0].Error()), 0o644)
				}
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := luacode.New(env).Compile(prog)
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(filepath.Join(goldenDir, name+".error"), []byte("compile: "+err.Error()), 0o644)
				}
				t.Fatalf("compile error: %v", err)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.lua")
			if err := os.WriteFile(file, code, 0o644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("lua", file)
			if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(in)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(filepath.Join(goldenDir, name+".error"), out, 0o644)
				}
				t.Fatalf("run error: %v\n%s", err, out)
			}
			got := bytes.TrimSpace(out)
			if shouldUpdate() {
				_ = os.WriteFile(filepath.Join(goldenDir, name+".lua"), code, 0o644)
				_ = os.WriteFile(filepath.Join(goldenDir, name+".out"), append(got, '\n'), 0o644)
				_ = os.Remove(filepath.Join(goldenDir, name+".error"))
				return
			}
			wantOut, err := os.ReadFile(filepath.Join(goldenDir, name+".out"))
			if err != nil {
				t.Fatalf("read golden out: %v", err)
			}
			if !bytes.Equal(got, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(wantOut))
			}
		})
	}
}
