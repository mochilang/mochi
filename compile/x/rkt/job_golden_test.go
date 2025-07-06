//go:build slow

package rktcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	rktcode "mochi/compile/x/rkt"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestRacketCompiler_JOB_Golden(t *testing.T) {
	if err := rktcode.EnsureRacket(); err != nil {
		t.Skipf("racket not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type error: %v", errs[0])
		}
		code, err := rktcode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile error: %v", err)
		}
		want, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "rkt", q+".rkt.out"))
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(want))
		}

		out, err := compileRun(t, src)
		if err != nil {
			t.Fatalf("%s: %v", q, err)
		}
		if wantRun, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "rkt", q+".out")); err == nil {
			if strings.TrimSpace(string(out)) != strings.TrimSpace(string(wantRun)) {
				t.Errorf("%s runtime mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, out, wantRun)
			}
		}
	}
}
