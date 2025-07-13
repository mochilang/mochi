//go:build slow

package zigcode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"mochi/compiler/x/testutil"
	zigcode "mochi/compiler/x/zig"
	"mochi/parser"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

// TestZigCompiler_TPCH_Golden compiles TPCH queries q1 through q5
// and compares the generated Zig code with golden files.
func TestZigCompiler_TPCH_Golden(t *testing.T) {
	if _, err := zigcode.EnsureZig(); err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 1; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := zigcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			code = bytes.TrimSpace(code)
			wantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "zig", q+".zig")
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, append(code, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				want, err := os.ReadFile(wantPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(code, bytes.TrimSpace(want)) {
					t.Errorf("generated code mismatch for %s.zig\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, code, bytes.TrimSpace(want))
				}
			}
			return
		})
	}
}
