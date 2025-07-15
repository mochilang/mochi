//go:build slow

package luacode_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"regexp"
	"sort"
	"strings"
	"testing"

	luacode "mochi/compiler/x/lua"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

var pathRE = regexp.MustCompile(`[^\s]*main\.lua`)

func sanitizeError(q string, b []byte) []byte {
	s := pathRE.ReplaceAll(b, []byte("/tmp/"+q+".lua"))
	s = bytes.ReplaceAll(s, []byte("<"), nil)
	return bytes.TrimSpace(s)
}

// TestLuaCompiler_TPCDS_Dataset_Golden compiles the TPC-DS examples and verifies
// the generated Lua code and program output.
func TestLuaCompiler_TPCDS_Dataset_Golden(t *testing.T) {
	if err := luacode.EnsureLua(); err != nil {
		t.Skipf("lua not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	dirEntries, err := os.ReadDir(filepath.Join(root, "tests", "dataset", "tpc-ds"))
	if err != nil {
		t.Fatal(err)
	}
	var queries []string
	for _, e := range dirEntries {
		name := e.Name()
		if strings.HasPrefix(name, "q") && strings.HasSuffix(name, ".mochi") {
			queries = append(queries, strings.TrimSuffix(name, ".mochi"))
		}
	}
	sort.Strings(queries)
	for _, q := range queries {
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "lua", q+".lua")
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
				t.Errorf("generated code mismatch for %s.lua\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, want)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.lua")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			errPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "lua", q+".error")
			out, _ := exec.Command("lua", file).CombinedOutput()
			gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
			if len(gotLines) == 0 {
				t.Fatalf("no output")
			}
			gotJSON := gotLines[0]
			if bytes.HasPrefix(gotJSON, []byte("lua:")) {
				if wantErr, err := os.ReadFile(errPath); err == nil {
					wantClean := sanitizeError(q, wantErr)
					gotClean := sanitizeError(q, out)
					if !bytes.Equal(gotClean, wantClean) {
						t.Errorf("error output mismatch for %s.error\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotClean, wantClean)
					}
					return
				}
				t.Skipf("runtime error: %s", gotJSON)
			}
			if _, err := os.Stat(errPath); err == nil {
				t.Errorf("expected runtime failure for %s", q)
			}
			wantOut, err := os.ReadFile(outWant)
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
