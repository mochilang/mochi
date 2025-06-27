//go:build slow

package cljcode_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	cljcode "mochi/compile/x/clj"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestClojureCompiler_JOBQ1(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "job", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := cljcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "clj", "q1.clj.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.clj.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.clj")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("clojure", file)
	cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("clojure run error: %v\n%s", err, out)
	}
	gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
	if len(gotLines) == 0 {
		t.Fatalf("no output")
	}
	gotJSON := gotLines[0]
	wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "clj", "q1.out"))
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
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotJSON, wantJSON)
	}
}
