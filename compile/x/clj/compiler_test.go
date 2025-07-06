//go:build slow

package cljcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cljcode "mochi/compile/x/clj"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestClojureCompiler_TwoSum(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	src := filepath.Join("..", "..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := cljcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
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
	got := strings.TrimSpace(string(out))
	want := "0\n1"
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}

func TestClojureCompiler_SubsetPrograms(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/clj", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}

		// compute expected output using the VM
		vmProg, err := vm.CompileWithSource(prog, env, string(mustRead(src)))
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		vmIn := bytes.NewReader(mustRead(strings.TrimSuffix(src, ".mochi") + ".in"))
		var vmOut bytes.Buffer
		m := vm.NewWithIO(vmProg, vmIn, &vmOut)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("❌ vm run error:\n%s", ve.Format(vmProg))
			}
			return nil, fmt.Errorf("❌ vm run error: %v", err)
		}
		want := bytes.TrimSpace(vmOut.Bytes())

		c := cljcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.clj")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("clojure", file)
		cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ clojure run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if !bytes.Equal(res, want) {
			return nil, fmt.Errorf("output mismatch\n-- clj --\n%s\n-- vm --\n%s", res, want)
		}
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestClojureCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/clj", ".mochi", ".clj.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := cljcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

// runLeetExample compiles the Mochi LeetCode solution for the given ID and runs
// the generated Clojure code. Any output is ignored; the test only checks that
// the program executes without error.
func runLeetExample(t *testing.T, id int) {
	dir := filepath.Join("..", "..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := cljcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.clj")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("clojure", file)
			cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("clojure run error: %v\n%s", err, out)
			}
		})
	}
}

func TestClojureCompiler_LeetCodeExamples(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	runLeetExample(t, 1)
	runLeetExample(t, 2)
	runLeetExample(t, 3)
	runLeetExample(t, 4)
	runLeetExample(t, 5)
	runLeetExample(t, 6)
	runLeetExample(t, 7)
	runLeetExample(t, 8)
	runLeetExample(t, 9)
	runLeetExample(t, 10)
}

func mustRead(path string) []byte {
	data, _ := os.ReadFile(path)
	return data
}
