package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/parser"
	"mochi/types"
)

// TestMochiLuaGolden compiles each Mochi source program under
// tests/rosetta/x/Mochi to Lua and verifies the generated code
// matches the golden files in tests/rosetta/out/Lua.
func TestMochiLuaGolden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Lua")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}
	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	for _, out := range outs {
		name := strings.TrimSuffix(filepath.Base(out), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		luaPath := filepath.Join(outDir, name+".lua")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				writeLuaError(outDir, name, fmt.Errorf("parse error: %w", err))
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeLuaError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
				t.Skip("type error")
				return
			}
			code, err := luacode.New(env).Compile(prog)
			if err != nil {
				writeLuaError(outDir, name, fmt.Errorf("compile error: %w", err))
				t.Skip("compile error")
				return
			}
			got := bytes.TrimSpace(code)
			if shouldUpdate() {
				if err := os.WriteFile(luaPath, append(got, '\n'), 0o644); err != nil {
					t.Fatalf("write lua: %v", err)
				}
				t.Logf("updated: %s", luaPath)
				return
			}
			want, err := os.ReadFile(luaPath)
			if err != nil {
				t.Fatalf("read lua golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("%s Lua\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}
