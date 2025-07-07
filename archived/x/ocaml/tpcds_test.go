//go:build archived && slow

package mlcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	ocamlcode "mochi/archived/x/ocaml"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestOCamlCompiler_TPCDS(t *testing.T) {
	if err := ocamlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocaml not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 8; i++ {
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
			code, err := ocamlcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "ocaml", q+".ml.out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				if os.IsNotExist(err) {
					if err := os.WriteFile(wantPath, bytes.TrimSpace(code), 0644); err != nil {
						t.Fatalf("write golden: %v", err)
					}
					t.Skipf("wrote golden %s", wantPath)
				}
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(want))
			}
			t.Skip("runtime disabled")
		})
	}
}
