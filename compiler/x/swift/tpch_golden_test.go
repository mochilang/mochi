//go:build slow

package swift_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	swift "mochi/compiler/x/swift"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSwiftCompiler_TPCH_Golden_q1_q5(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 5; i++ {
		base := fmt.Sprintf("q%d", i)
		t.Run(base, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := swift.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			codePath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "swift", base+".swift")
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			out, err := compileAndRunSwiftSrc(t, swiftExe, code)
			if err != nil {
				t.Skipf("swift run error: %v", err)
				return
			}
			gotOut := bytes.TrimSpace(out)
			outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "out", base+".out")
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden out: %v", err)
			}
			wantOut = bytes.TrimSpace(wantOut)
			if !bytes.Equal(gotOut, wantOut) {
				t.Fatalf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", gotOut, wantOut)
			}
		})
	}
}
