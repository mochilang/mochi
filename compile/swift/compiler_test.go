package swiftcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	swiftcode "mochi/compile/swift"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_TwoSum(t *testing.T) {
	if _, err := exec.LookPath("swift"); err != nil {
		t.Skip("swift not installed")
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := swiftcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("swiftc", file, "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("swiftc error: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("swift run error: %v\n%s", err, out)
	}
	res := bytes.TrimSpace(out)
	if string(res) != "0\n1" {
		t.Fatalf("unexpected output: %s", res)
	}
}
