package ccode_test

import (
	"os"
	"os/exec"
	"testing"

	ccode "mochi/compile/c"
	"mochi/parser"
	"mochi/types"
)

// TestBasic ensures the C compiler can compile a trivial program and run it using gcc.
func TestBasic(t *testing.T) {
	if _, err := exec.LookPath("gcc"); err != nil {
		t.Skip("gcc not installed")
	}
	src := "print(\"hi\")"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := ccode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	srcFile := tmp + "/main.c"
	if err := os.WriteFile(srcFile, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exeFile := tmp + "/a.out"
	cmd := exec.Command("gcc", srcFile, "-o", exeFile)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("gcc failed: %v\n%s", err, out)
	}
	out, err := exec.Command(exeFile).CombinedOutput()
	if err != nil {
		t.Fatalf("program failed: %v\n%s", err, out)
	}
	got := string(out)
	if got != "hi\n" {
		t.Fatalf("unexpected output: %q", got)
	}
}
