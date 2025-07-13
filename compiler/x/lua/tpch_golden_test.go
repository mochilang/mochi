package luacode_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_TPCH_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	// Only verify a couple of queries as a sanity check.
	for _, q := range []string{"q1", "q2"} {
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
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
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "lua", q+".lua")
		wantCode, err := os.ReadFile(codeWant)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		strip := func(b []byte) []byte {
			b = bytes.TrimSpace(b)
			if i := bytes.IndexByte(b, '\n'); i >= 0 && bytes.HasPrefix(b, []byte("-- Generated")) {
				return b[i+1:]
			}
			return b
		}
		got := strip(code)
		want := strip(wantCode)
		if !bytes.Equal(got, want) {
			t.Errorf("generated code mismatch for %s.lua\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, got, want)
		}
		dir := t.TempDir()
		srcFile := filepath.Join(dir, "main.lua")
		if err := os.WriteFile(srcFile, code, 0644); err != nil {
			t.Fatalf("write error: %v", err)
		}
		out, err := exec.Command("lua", srcFile).CombinedOutput()
		if err != nil {
			t.Fatalf("lua error: %v\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "lua", q+".out"))
		if err != nil {
			t.Fatalf("read golden output: %v", err)
		}
		var gotVal, wantVal any
		if err := json.Unmarshal(gotOut, &gotVal); err != nil {
			t.Fatalf("parse got json: %v", err)
		}
		if err := json.Unmarshal(bytes.TrimSpace(wantOut), &wantVal); err != nil {
			t.Fatalf("parse want json: %v", err)
		}
		if !reflect.DeepEqual(gotVal, wantVal) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", q, gotOut, bytes.TrimSpace(wantOut))
		}
	}
}
