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

// TestZigCompiler_JOB_Golden compiles JOB query q1 and compares the
// generated Zig code with the golden file. The compiled executable is
// then run and its output compared with the VM.
func TestZigCompiler_JOB_Golden(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 11; i <= 20; i++ {
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
			code, err := zigcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			gotCode := stripHeader(bytes.TrimSpace(code))
			wantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "zig", q+".zig")
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, append(gotCode, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				want, err := os.ReadFile(wantPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotCode, stripHeader(bytes.TrimSpace(want))) {
					t.Errorf("generated code mismatch for %s.zig\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotCode, bytes.TrimSpace(stripHeader(want)))
				}
			}

			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.zig")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(tmp, "main")
			if out, err := exec.Command(zigc, "build-exe", file, "-O", "ReleaseSafe", "-femit-bin="+exe).CombinedOutput(); err != nil {
				t.Fatalf("zig build error: %v\n%s", err, out)
			}
			out, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			compiled := bytes.TrimSpace(out)

			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("vm compile error: %v", err)
			}
			var buf bytes.Buffer
			m := vm.New(p, &buf)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					t.Fatalf("vm run error:\n%s", ve.Format(p))
				}
				t.Fatalf("vm run error: %v", err)
			}
			vmOut := bytes.TrimSpace(buf.Bytes())
			if !bytes.Equal(compiled, vmOut) {
				t.Errorf("output mismatch for %s\n\n-- zig --\n%s\n\n-- vm --\n%s\n", q, compiled, vmOut)
			}

			wantOutPath := filepath.Join(root, "tests", "dataset", "job", "out", q+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(compiled, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".out", compiled, bytes.TrimSpace(wantOut))
			}
		})
	}
}
