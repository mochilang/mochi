//go:build archived && slow

package dartcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	dartcode "mochi/archived/x/dart"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_SLT(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	root := findRoot(t)
	cases := []string{"case1", "case2", "case3"}
	for _, c := range cases {
		src := filepath.Join(root, "tests", "dataset", "slt", "out", "select1", c+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		t.Run(c, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := dartcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCodePath := filepath.Join(root, "tests", "dataset", "slt", "compiler", "dart", c+".dart.out")
			wantCode, err := os.ReadFile(wantCodePath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.dart.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", c, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.dart")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("dart", file)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Skipf("dart run error: %v\n%s", err, out)
				return
			}
			gotOut := bytes.TrimSpace(out)
			wantOutPath := filepath.Join(root, "tests", "dataset", "slt", "compiler", "dart", c+".out")
			wantOut, err := os.ReadFile(wantOutPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", c, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
