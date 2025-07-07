//go:build archived && slow

package schemecode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	schemecode "mochi/archived/x/scheme"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestSchemeCompiler_TPCDS(t *testing.T) {
	if _, err := schemecode.EnsureScheme(); err != nil {
		t.Skipf("scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	ranges := [][]int{{10, 49}, {50, 70}, {79, 99}}
	skip := map[int]bool{40: true, 77: true, 78: true, 93: true, 97: true}
	for _, r := range ranges {
		for i := r[0]; i <= r[1]; i++ {
			if skip[i] {
				continue
			}
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
				code, err := schemecode.New(env).Compile(prog)
				if err != nil {
					t.Fatalf("compile error: %v", err)
				}
				wantCode, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "scheme", q+".scm.out"))
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s.scm.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
				}
				dir := t.TempDir()
				file := filepath.Join(dir, "main.scm")
				if err := os.WriteFile(file, code, 0644); err != nil {
					t.Fatalf("write error: %v", err)
				}
				t.Skip("Scheme runtime check disabled until missing libraries are available")
			})
		}
	}
}
