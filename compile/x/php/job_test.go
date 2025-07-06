//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	phpcode "mochi/compile/x/php"
	"mochi/compile/x/testutil"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestPHPCompiler_JOB(t *testing.T) {
	if err := phpcode.EnsurePHP(); err != nil {
		t.Skipf("php not installed: %v", err)
	}
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
			code, err := phpcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.php")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			codeWantPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "php", q+".php.out")
			if codeWant, err := os.ReadFile(codeWantPath); err == nil {
				if string(code) != string(codeWant) {
					t.Fatalf("generated code mismatch")
				}
			}
			cmd := exec.Command("php", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("php run error: %v\n%s", err, out)
			}
			phpOut := bytes.TrimSpace(out)

			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("vm compile error: %v", err)
			}
			var vmBuf bytes.Buffer
			m := vm.New(p, &vmBuf)
			if err := m.Run(); err != nil {
				if ve, ok := err.(*vm.VMError); ok {
					t.Fatalf("vm run error:\n%s", ve.Format(p))
				}
				t.Fatalf("vm run error: %v", err)
			}
			vmOut := bytes.TrimSpace(vmBuf.Bytes())
			if !bytes.Equal(phpOut, vmOut) {
				t.Fatalf("vm mismatch\n\n--- PHP ---\n%s\n\n--- VM ---\n%s", phpOut, vmOut)
			}

			wantData, err := os.ReadFile(filepath.Join(root, "tests", "dataset", "job", "compiler", "php", q+".out"))
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if string(phpOut) != string(bytes.TrimSpace(wantData)) {
				t.Fatalf("unexpected output\n--- got ---\n%s\n--- want ---\n%s", phpOut, bytes.TrimSpace(wantData))
			}
		})
	}
}
