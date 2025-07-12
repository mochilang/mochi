//go:build slow

package cljcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cljcode "mochi/compiler/x/clj"
	"mochi/parser"
	"mochi/types"
)

func TestCLJCompiler_TPCHQueries(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 2; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", base+".mochi")
		codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "clj", base+".clj.out")
		outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "clj", base+".out")
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
			code, err := cljcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.clj.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.clj")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("clojure", file)
			cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("clojure run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
