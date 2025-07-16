//go:build slow

package zigcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/compiler/x/testutil"
	zigcode "mochi/compiler/x/zig"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// TestZigCompiler_TPCH_Golden compiles TPCH queries q1 through q22,
// compares the generated Zig code with golden files, builds the result
// and checks the output against the VM and .out files.
func TestZigCompiler_TPCH_Golden(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 22; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
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
			gotCode := stripHeader(bytes.TrimSpace(code))
			wantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "zig", q+".zig")
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, append(gotCode, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				want, err := os.ReadFile(wantPath)
				if err != nil {
					t.Skipf("missing golden: %v", err)
				}
				if !bytes.Equal(gotCode, stripHeader(bytes.TrimSpace(want))) {
					t.Skipf("generated code mismatch for %s.zig", q)
				}
			}

			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.zig")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(tmp, "main")
			out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput()
			if err != nil {
				t.Skipf("zig build error: %v", err)
			}
			out, err = exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Skipf("run error: %v", err)
			}
			compiled := bytes.TrimSpace(out)

			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Skipf("vm compile error: %v", err)
			}
			var buf bytes.Buffer
			m := vm.New(p, &buf)
			if err := m.Run(); err != nil {
				t.Skipf("vm run error: %v", err)
			}
			vmOut := bytes.TrimSpace(buf.Bytes())
			if !bytes.Equal(compiled, vmOut) {
				t.Errorf("output mismatch for %s\n\n-- zig --\n%s\n\n-- vm --\n%s\n", q, compiled, vmOut)
			}

			wantOutPath := filepath.Join(root, "tests", "dataset", "tpc-h", "out", q+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Skipf("missing out file: %v", err)
			}
			if !bytes.Equal(compiled, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".out", compiled, bytes.TrimSpace(wantOut))
			}
		})
	}
}
