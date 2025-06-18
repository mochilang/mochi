//go:build slow

package scalacode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	scalacode "mochi/compile/scala"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestScalaCompiler_LeetCodeExample1 compiles the LeetCode two-sum example
// and verifies the generated Scala program runs as expected.
func TestScalaCompiler_LeetCodeExample1(t *testing.T) {
	runLeetExample(t, 1)
}

// TestScalaCompiler_LeetCodeExample2 compiles the add-two-numbers example and
// verifies the generated Scala program runs without errors.
func TestScalaCompiler_LeetCodeExample2(t *testing.T) {
	runLeetExample(t, 2)
}

// TestScalaCompiler_LeetCodeExample3 compiles the longest-substring example
// and ensures the generated Scala code runs correctly.
func TestScalaCompiler_LeetCodeExample3(t *testing.T) {
	runLeetExample(t, 3)
}

func TestScalaCompiler_SubsetPrograms(t *testing.T) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/valid_scala", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.scala")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		if out, err := exec.Command("scalac", file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ scalac error: %w\n%s", err, out)
		}

		scalaCmd := "scala"
		args := []string{"Main"}
		if _, err := exec.LookPath("scala-cli"); err == nil {
			scalaCmd = "scala-cli"
			args = []string{"run", file}
		} else if out, err := exec.Command("scala", "-version").CombinedOutput(); err == nil && bytes.Contains(out, []byte("Scala CLI")) {
			args = []string{"run", file}
		}
		cmd := exec.Command(scalaCmd, args...)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ scala run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
	golden.Run(t, "tests/compiler/scala", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.scala")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		if out, err := exec.Command("scalac", file).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ scalac error: %w\n%s", err, out)
		}

		scalaCmd := "scala"
		args := []string{"Main"}
		if _, err := exec.LookPath("scala-cli"); err == nil {
			scalaCmd = "scala-cli"
			args = []string{"run", file}
		} else if out, err := exec.Command("scala", "-version").CombinedOutput(); err == nil && bytes.Contains(out, []byte("Scala CLI")) {
			args = []string{"run", file}
		}
		cmd := exec.Command(scalaCmd, args...)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ scala run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestScalaCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid_scala", ".mochi", ".scala.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
	golden.Run(t, "tests/compiler/scala", ".mochi", ".scala.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := scalacode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

// runLeetExample compiles the Mochi program under examples/leetcode/<id>
// using the Scala backend and executes the resulting binary. It fails the test
// if the program does not run successfully.
func runLeetExample(t *testing.T, id int) {
	if err := scalacode.EnsureScala(); err != nil {
		t.Skipf("scala not installed: %v", err)
	}
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no examples found in %s", dir)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := scalacode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "Main.scala")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			if out, err := exec.Command("scalac", file).CombinedOutput(); err != nil {
				t.Fatalf("scalac error: %v\n%s", err, out)
			}

			scalaCmd := "scala"
			args := []string{"Main"}
			if _, err := exec.LookPath("scala-cli"); err == nil {
				scalaCmd = "scala-cli"
				args = []string{"run", file}
			} else if out, err := exec.Command("scala", "-version").CombinedOutput(); err == nil && bytes.Contains(out, []byte("Scala CLI")) {
				args = []string{"run", file}
			}
			cmd := exec.Command(scalaCmd, args...)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("scala run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}
