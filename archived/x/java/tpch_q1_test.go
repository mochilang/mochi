//go:build archived && slow

package javacode_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	javacode "mochi/archived/x/java"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestJavaCompiler_TPCHQ1(t *testing.T) {
	if err := javacode.EnsureJavac(); err != nil {
		t.Skipf("javac not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := javacode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "java", "q1.java.out"))
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.java.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "Main.java")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	if out, err := exec.Command("javac", "-d", dir, file).CombinedOutput(); err != nil {
		t.Fatalf("javac error: %v\n%s", err, out)
	}
	cmd := exec.Command("java", "-cp", dir, "Main")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("java run error: %v\n%s", err, out)
	}
	gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
	if len(gotLines) == 0 {
		t.Fatalf("no output")
	}
	gotJSON := gotLines[0]
	wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "java", "q1.out"))
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
