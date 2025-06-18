//go:build slow

package zigcode_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	zigcode "mochi/compile/zig"
	"mochi/parser"
	"mochi/types"
)

// TestZigCompiler_TwoSum compiles the LeetCode example using zig cc and runs it.
func TestZigCompiler_TwoSum(t *testing.T) {
	zig, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
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
	binBytes, err := zigcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	bin := filepath.Join(dir, "prog")
	if err := os.WriteFile(bin, binBytes, 0755); err != nil {
		t.Fatalf("write binary: %v", err)
	}
	cmd := exec.Command(bin)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
	_ = zig // avoid unused variable if build tags skip test
}
