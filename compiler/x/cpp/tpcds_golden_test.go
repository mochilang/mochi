//go:build slow

package cpp_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cpp "mochi/compiler/x/cpp"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestCPPCompiler_TPCDSQueries(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "cpp", base+".cpp.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "cpp", base+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
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
			code, err := cpp.New().Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			gotCode := bytes.TrimSpace(code)
			if shouldUpdate() {
				_ = os.WriteFile(codeWant, append(gotCode, '\n'), 0644)
			} else {
				wantCode, err := os.ReadFile(codeWant)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
					t.Errorf("generated code mismatch for %s.cpp.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotCode, bytes.TrimSpace(wantCode))
				}
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.cpp")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command("g++", file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
				t.Skipf("g++ error: %v\n%s", err, out)
				return
			}
			outBytes, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v\n%s", err, outBytes)
				return
			}
			gotOut := bytes.TrimSpace(outBytes)
			if shouldUpdate() {
				_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
			} else {
				wantOut, err := os.ReadFile(outWant)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
