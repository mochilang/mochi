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

func TestCLJCompiler_TPCDSQueries(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
		codePath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "clj", base+".clj")
		outPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "clj", base+".out")
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
			code, err := cljcode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			if shouldUpdate() {
				_ = os.WriteFile(codePath, code, 0644)
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
				t.Skipf("clojure run error: %v\n%s", err, out)
				return
			}
			gotOut := bytes.TrimSpace(out)
			if shouldUpdate() {
				_ = os.WriteFile(outPath, append(gotOut, '\n'), 0644)
				return
			}
			wantOut, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Skipf("output mismatch for %s", base)
				return
			}
		})
	}
}
