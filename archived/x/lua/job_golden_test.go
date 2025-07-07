//go:build archived && slow

package luacode_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strconv"
	"testing"

	luacode "mochi/archived/x/lua"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_JOB_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := "q" + strconv.Itoa(i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
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
			wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "lua", q+".lua.out"))
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.lua.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
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
			gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
			if len(gotLines) == 0 {
				t.Fatalf("no output")
			}
			gotJSON := gotLines[0]
			wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "lua", q+".out"))
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantLines := bytes.Split(bytes.TrimSpace(wantOut), []byte("\n"))
			wantJSON := wantLines[0]
			var gotVal, wantVal any
			if err := json.Unmarshal(gotJSON, &gotVal); err != nil {
				t.Fatalf("parse got json: %v", err)
			}
			if err := json.Unmarshal(wantJSON, &wantVal); err != nil {
				t.Fatalf("parse want json: %v", err)
			}
			if !reflect.DeepEqual(gotVal, wantVal) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotJSON, wantJSON)
			}
		})
	}
}
