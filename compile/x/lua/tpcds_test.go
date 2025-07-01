//go:build slow

package luacode_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	luacode "mochi/compile/x/lua"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestLuaCompiler_TPCDS_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	updateFlag := flag.Lookup("update")
	update := updateFlag != nil && updateFlag.Value.String() == "true"
	for _, q := range []string{"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19", "q20", "q21", "q22", "q23", "q24", "q25", "q26", "q27", "q28", "q29"} {
		q := q
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
			codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "lua", q+".lua.out")
			if update {
				if err := os.WriteFile(codePath, bytes.TrimSpace(code), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				wantCode, err := os.ReadFile(codePath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s.lua.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
				}
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
			outPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "lua", q+".out")
			if update {
				if err := os.WriteFile(outPath, bytes.TrimSpace(out), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				return
			}
			gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
			if len(gotLines) == 0 {
				t.Fatalf("no output")
			}
			gotJSON := gotLines[0]
			wantOut, err := os.ReadFile(outPath)
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
