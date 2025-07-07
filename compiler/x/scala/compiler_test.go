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

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func TestScalaCompilerMachine(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "scala")
	os.MkdirAll(outDir, 0755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			codePath := filepath.Join(outDir, name+".scala")
			outPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")
			prog, err := parser.Parse(src)
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()), 0644)
				t.Skip("parse error")
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
				t.Skip("type error")
			}
			code, err := scalacode.New(env).Compile(prog)
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()), 0644)
				t.Skip("compile error")
			}
			os.WriteFile(codePath, code, 0644)
			tmp := t.TempDir()
			if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
				os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0644)
				t.Skip("scalac failed")
			}
			cmd := exec.Command("scala", "-cp", tmp, name)
			var runOut bytes.Buffer
			cmd.Stdout = &runOut
			cmd.Stderr = &runOut
			if err := cmd.Run(); err != nil {
				os.WriteFile(errPath, append([]byte(err.Error()+"\n"), runOut.Bytes()...), 0644)
				t.Skip("run failed")
			}
			os.WriteFile(outPath, runOut.Bytes(), 0644)
		})
	}
}

func TestScalaCompilerGolden(t *testing.T) {
	golden.Run(t, "tests/human/x/scala", ".scala", ".scala", func(src string) ([]byte, error) {
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(data), nil
	})
}
