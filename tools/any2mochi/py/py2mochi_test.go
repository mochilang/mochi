//go:build slow

package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

var update = false

func TestPy2Mochi(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "compiler", "py", "*.py.out")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no test files found")
	}
	for _, pyFile := range files {
		name := strings.TrimSuffix(filepath.Base(pyFile), ".py.out")
		t.Run(name, func(t *testing.T) {
			if name == "dataset_negative_skip_take" {
				t.Skip("negative skip/take not supported")
			}
			goldenPath := filepath.Join(root, "tests", "compiler", "py", name+".mochi.out")
			if _, err := os.Stat(goldenPath); err != nil {
				t.Skip("missing golden for " + name)
			}
			outPath := filepath.Join(root, "tests", "compiler", "py", name+".out")
			if _, err := os.Stat(outPath); err != nil {
				t.Skip("missing output for " + name)
			}
			cmd := exec.Command("python3", filepath.Join(root, "tools", "any2mochi", "py", "py2mochi.py"), pyFile)
			mochiCode, err := cmd.Output()
			if err != nil {
				t.Fatalf("py2mochi error: %v", err)
			}
			if update {
				if err := os.WriteFile(goldenPath, mochiCode, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				want, err := os.ReadFile(goldenPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if !bytes.Equal(bytes.TrimSpace(mochiCode), bytes.TrimSpace(want)) {
					t.Errorf("generated mochi mismatch for %s", name)
				}
			}

			// run python to get output
			pyCmd := exec.Command("python3", pyFile)
			if data, err := os.ReadFile(filepath.Join(filepath.Dir(pyFile), name+".in")); err == nil {
				pyCmd.Stdin = bytes.NewReader(data)
			}
			pyOut, err := pyCmd.CombinedOutput()
			if err != nil {
				t.Fatalf("python run error: %v\n%s", err, pyOut)
			}

			// run mochi interpreter on generated code
			out, err := runMochi(string(mochiCode), root, filepath.Join(filepath.Dir(pyFile), name+".in"))
			if err != nil {
				t.Fatalf("mochi run error: %v", err)
			}
			if !bytes.Equal(bytes.TrimSpace(pyOut), bytes.TrimSpace(out)) {
				t.Errorf("output mismatch for %s\nPython: %s\nMochi: %s", name, bytes.TrimSpace(pyOut), bytes.TrimSpace(out))
			}
		})
	}
}

func runMochi(code string, root string, inFile string) ([]byte, error) {
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, errs[0]
	}
	var out bytes.Buffer
	interp := interpreter.New(prog, env, root)
	interp.Env().SetWriter(&out)
	if data, err := os.ReadFile(inFile); err == nil {
		interp.Env().SetReader(bytes.NewReader(data))
	}
	if err := interp.Run(); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

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
