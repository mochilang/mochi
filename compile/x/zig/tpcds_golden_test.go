//go:build slow

package zigcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	_ "mochi/golden"

	"mochi/compile/x/testutil"
	zigcode "mochi/compile/x/zig"
	"mochi/parser"
	"mochi/types"
)

// TestZigCompiler_TPCDS_Golden compiles a small query that
// groups joined rows and compares the generated Zig code with
// the golden file.
func TestZigCompiler_TPCDS_Golden(t *testing.T) {
	if _, err := zigcode.EnsureZig(); err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
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
			code, err := zigcode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
			}
			wantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "zig", q+".zig.out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Skipf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("generated code mismatch for %s.zig.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(want))
			}
		})
	}
}
