//go:build archived && slow

package cobolcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	cobolcode "mochi/archived/x/cobol"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCobolCompiler_JOB_Golden(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	os.Setenv("MOCHI_SKIP_COBFMT", "1")
	defer os.Unsetenv("MOCHI_SKIP_COBFMT")
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
			code, err := cobolcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cobol", q+".cob.out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".cob.out", got, bytes.TrimSpace(want))
			}
		})
	}
}
