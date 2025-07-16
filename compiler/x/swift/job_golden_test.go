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

func TestSwiftCompiler_JOB_Golden_q11_q20(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	passed, failed := 0, 0
	for i := 11; i <= 20; i++ {
		base := fmt.Sprintf("q%d", i)
		codePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "swift", base+".swift")
		outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "swift", base+".out")
		if _, err := os.Stat(outWant); err != nil {
			continue
		}
		ok := t.Run(base, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", base+".mochi")
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
			_ = os.WriteFile(codePath, code, 0644)
			out, err := compileAndRunSwiftSrc(t, swiftExe, code)
			if err != nil {
				t.Fatalf("swift run error: %v", err)
			}
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden out: %v", err)
			}
			if got, want := bytes.TrimSpace(out), bytes.TrimSpace(wantOut); !bytes.Equal(got, want) {
				t.Fatalf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, want)
			}
		})
		if ok {
			passed++
		} else {
			failed++
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
}
