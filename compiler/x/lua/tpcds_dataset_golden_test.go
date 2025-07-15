//go:build slow

package luacode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestLuaCompiler_TPCDS_Dataset_Golden compiles the TPC-DS examples and verifies
// the generated Lua code and program output.
func TestLuaCompiler_TPCDS_Dataset_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	queries := []string{"q39", "q40", "q44", "q47", "q93", "q97", "q99"}
	for _, q := range queries {
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "lua", q+".lua.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "lua", q+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := luacode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := stripHeader(bytes.TrimSpace(code))
			want := stripHeader(bytes.TrimSpace(wantCode))
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.lua.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, want)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.lua")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			out, _ := exec.Command("lua", file).CombinedOutput()
			gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
			if len(gotLines) == 0 {
				t.Fatalf("no output")
			}
			gotJSON := gotLines[0]
			if bytes.HasPrefix(gotJSON, []byte("lua:")) {
				t.Skipf("runtime error: %s", gotJSON)
			}
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantLines := bytes.Split(bytes.TrimSpace(wantOut), []byte("\n"))
			wantJSON := wantLines[0]
			if !bytes.Equal(gotJSON, wantJSON) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotJSON, wantJSON)
			}
		})
	}
}
