package cljcode_test

import (
	"bytes"
	"fmt"
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

func TestClojureCompiler_VMValid_Golden(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "clj")
	os.MkdirAll(outDir, 0o755)

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		code, err := cljcode.New(env).Compile(prog)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("compile: "+err.Error()), 0o644)
			return nil, fmt.Errorf("compile error: %w", err)
		}
		cljFile := filepath.Join(outDir, base+".clj")
		if err := os.WriteFile(cljFile, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("clojure", cljFile)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, base+".error"), out, 0o644)
			return nil, fmt.Errorf("run error: %w", err)
		}
		out = bytes.TrimSpace(out)
		_ = os.WriteFile(filepath.Join(outDir, base+".out"), out, 0o644)
		_ = os.Remove(filepath.Join(outDir, base+".error"))
		return out, nil
	})
}
