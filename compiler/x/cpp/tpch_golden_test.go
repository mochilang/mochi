//go:build slow

package cpp_test

import (
	"bytes"
	"flag"
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

var update = flag.Bool("update", false, "update golden files")

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestCPPCompiler_TPCHQueries(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		base := fmt.Sprintf("q%d", i)
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cpp", base+".cpp")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cpp", base+".out")
		if _, err := os.Stat(codeWant); err != nil {
			continue
		}
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
			code, err := cpp.New().Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
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
					t.Errorf("generated code mismatch for %s.cpp\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", base, gotCode, bytes.TrimSpace(wantCode))
				}
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "prog.cpp")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			bin := filepath.Join(dir, "prog")
			if out, err := exec.Command("g++", file, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
				t.Fatalf("g++ error: %v\n%s", err, out)
			}
			outBytes, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, outBytes)
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
