//go:build slow

package scalacode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	scalacode "mochi/compiler/x/scala"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestScalaCompilerVMValid(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "scala")
	os.MkdirAll(outDir, 0o755)

	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".scala")
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
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command("scala", "-cp", tmp, name)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		var runOut bytes.Buffer
		cmd.Stdout = &runOut
		cmd.Stderr = &runOut
		if err := cmd.Run(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), runOut.Bytes()...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(runOut.Bytes())
		os.WriteFile(outPath, outBytes, 0o644)
		os.Remove(errPath)
		return outBytes, nil
	})
}
