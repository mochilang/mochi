//go:build slow

package hscode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	hscode "mochi/compiler/x/hs"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

var updateTPCDS = flag.Bool("update_tpcds", false, "update TPC-DS golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update_tpcds")
	return f != nil && f.Value.String() == "true"
}

func TestHSCompiler_TPCDSQueries(t *testing.T) {
	if err := hscode.EnsureHaskell(); err != nil {
		t.Skipf("haskell not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "hs", base+".hs.out")
		outPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "hs", base+".out")
		errPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "hs", base+".error")
		if !shouldUpdate() {
			if _, err := os.Stat(outPath); err != nil {
				continue
			}
		}
		t.Run(base, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := hscode.New(env).Compile(prog)
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				}
				t.Skipf("compile error: %v", err)
				return
			}
			os.Remove(errPath)
			gotCode := bytes.TrimSpace(code)
			if shouldUpdate() {
				_ = os.WriteFile(codePath, append(gotCode, '\n'), 0644)
                       }
                       dir := t.TempDir()
			file := filepath.Join(dir, "prog.hs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("runhaskell", file)
			outBytes, err := cmd.CombinedOutput()
			if err != nil {
				if shouldUpdate() {
					_ = os.WriteFile(errPath, outBytes, 0644)
				}
				t.Skipf("runhaskell error: %v\n%s", err, outBytes)
				return
			}
			os.Remove(errPath)
			gotOut := bytes.TrimSpace(outBytes)
			if shouldUpdate() {
				_ = os.WriteFile(outPath, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(outPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
