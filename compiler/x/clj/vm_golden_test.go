package cljcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cljcode "mochi/compiler/x/clj"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestClojureCompilerVMValid(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "clj")
	os.MkdirAll(outDir, 0o755)

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".clj")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		code, err := cljcode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("clojure", codePath)
		cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		out = bytes.TrimSpace(out)
		os.WriteFile(outPath, out, 0o644)
		os.Remove(errPath)
		return out, nil
	})
}
