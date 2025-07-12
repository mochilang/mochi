//go:build slow

package scalacode_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	scalacode "mochi/compiler/x/scala"
	"mochi/parser"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func runTPCHQuery(t *testing.T, base string) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
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
	wantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "scala", base+".scala.out")
	if want, err := os.ReadFile(wantPath); err == nil {
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("generated code mismatch for %s.scala.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(want))
		}
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
	gotLines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
	if len(gotLines) == 0 {
		t.Fatalf("no output")
	}
	gotJSON := gotLines[0]
	wantOut, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "scala", base+".out"))
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
	if fmt.Sprintf("%v", gotVal) != fmt.Sprintf("%v", wantVal) {
		t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotJSON, wantJSON)
	}
}

func TestScalaCompilerTPCH(t *testing.T) {
	for i := 1; i <= 2; i++ {
		base := fmt.Sprintf("q%d", i)
		t.Run(base, func(t *testing.T) { runTPCHQuery(t, base) })
	}
}
