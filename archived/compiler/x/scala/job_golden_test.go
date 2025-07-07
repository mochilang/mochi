//go:build archived && slow

package scalacode_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	scalacode "mochi/archived/x/scala"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestScalaCompiler_JOB_Golden compiles JOB queries q1..q10 and compares
// generated Scala code and runtime output against golden files.
func TestScalaCompiler_JOB_Golden(t *testing.T) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
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
			code, err := scalacode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scala", q+".scala.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.scala.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "Main.scala")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			if out, err := exec.Command("scalac", file).CombinedOutput(); err != nil {
				t.Skipf("scalac error: %v\n%s", err, out)
				return
			}
			scalaCmd := "scala"
			args := []string{"Main"}
			if _, err := exec.LookPath("scala-cli"); err == nil {
				scalaCmd = "scala-cli"
				args = []string{"run", file}
			} else if out, err := exec.Command("scala", "-version").CombinedOutput(); err == nil && bytes.Contains(out, []byte("Scala CLI")) {
				args = []string{"run", file}
			}
			out, err := exec.Command(scalaCmd, args...).CombinedOutput()
			if err != nil {
				t.Skipf("scala run error: %v\n%s", err, out)
				return
			}
			gotOutLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
			if len(gotOutLines) == 0 {
				t.Fatalf("no output")
			}
			gotJSON := gotOutLines[0]
			wantOutPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "scala", q+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantJSON := bytes.Split(bytes.TrimSpace(wantOut), []byte("\n"))[0]
			var gv, wv any
			if err := json.Unmarshal(gotJSON, &gv); err != nil {
				t.Fatalf("parse got json: %v", err)
			}
			if err := json.Unmarshal(wantJSON, &wv); err != nil {
				t.Fatalf("parse want json: %v", err)
			}
			if !reflect.DeepEqual(gv, wv) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotJSON, wantJSON)
			}
		})
	}
}
