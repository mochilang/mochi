//go:build slow

package zigcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler/x/testutil"
	zigcode "mochi/compiler/x/zig"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestZigCompiler_VMValid_Golden(t *testing.T) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		t.Skipf("zig not installed: %v", err)
	}
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")

	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
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
			codePath := filepath.Join(root, "tests", "machine", "x", "zig", name+".zig")
			if shouldUpdate() {
				if err := os.WriteFile(codePath, append(gotCode, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				want, err := os.ReadFile(codePath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(gotCode, stripHeader(bytes.TrimSpace(want))) {
					t.Errorf("generated code mismatch for %s.zig\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotCode, bytes.TrimSpace(stripHeader(want)))
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
			cmd := exec.Command(exe)
			if inData, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(inData)
			}
			out, err := cmd.CombinedOutput()
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
				t.Errorf("output mismatch for %s\n\n-- zig --\n%s\n\n-- vm --\n%s\n", name, compiled, vmOut)
			}

			outPath := filepath.Join(root, "tests", "machine", "x", "zig", name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, append(compiled, '\n'), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			} else {
				wantOut, err := os.ReadFile(outPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(compiled, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, compiled, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}
