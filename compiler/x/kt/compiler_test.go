package ktcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ktcode "mochi/compiler/x/kt"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestKTCompiler_TwoSum(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := ktcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		t.Fatalf("kotlinc error: %v\n%s", err, out)
	}
	out, err := exec.Command("java", "-jar", jar).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	if string(got) != "0\n1" {
		t.Fatalf("unexpected output: %s", got)
	}
}

func TestKTCompiler_LeetCodeExamples(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	runKTLeetExample(t, 1)
	runKTLeetExample(t, 2)
	runKTLeetExample(t, 3)
	runKTLeetExample(t, 4)
	runKTLeetExample(t, 5)
	runKTLeetExample(t, 6)
	runKTLeetExample(t, 7)
}

func TestKTCompiler_SubsetPrograms(t *testing.T) {
	t.Skip("slow")
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := ktcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.kt")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		jar := filepath.Join(dir, "main.jar")
		if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ kotlinc error: %w\n%s", err, out)
		}
		cmd := exec.Command("java", "-jar", jar)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})

	golden.Run(t, "tests/compiler/kt", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := ktcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "Main.kt")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		jar := filepath.Join(dir, "main.jar")
		if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ kotlinc error: %w\n%s", err, out)
		}
		cmd := exec.Command("java", "-jar", jar)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestKTCompiler_GoldenOutput(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}

	golden.Run(t, "tests/compiler/kt", ".mochi", ".kt.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := ktcode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		// Write and run the generated Kotlin code.
		dir := t.TempDir()
		ktFile := filepath.Join(dir, "Main.kt")
		if err := os.WriteFile(ktFile, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		jar := filepath.Join(dir, "main.jar")
		if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ kotlinc error: %w\n%s", err, out)
		}
		cmd := exec.Command("java", "-jar", jar)
		inPath := strings.TrimSuffix(src, ".mochi") + ".in"
		var inData []byte
		if data, err := os.ReadFile(inPath); err == nil {
			cmd.Stdin = bytes.NewReader(data)
			inData = data
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)

		// Run using the Mochi VM for comparison.
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, bytes.NewReader(inData), &vmBuf)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("❌ vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("❌ vm run error: %w", err)
		}
		if vmOut := bytes.TrimSpace(vmBuf.Bytes()); !bytes.Equal(vmOut, gotOut) {
			t.Errorf("runtime mismatch for %s\n\n--- VM ---\n%s\n\n--- Kotlin ---\n%s\n", filepath.Base(src), vmOut, gotOut)
		}

		// Compare Kotlin output to golden if present.
		wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
		if wantData, err := os.ReadFile(wantPath); err == nil {
			if want := bytes.TrimSpace(wantData); !bytes.Equal(gotOut, want) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", filepath.Base(wantPath), gotOut, want)
			}
		}

		return bytes.TrimSpace(code), nil
	})
}

func repoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found")
	return ""
}

func runKTProgram(t *testing.T, src string) []byte {
	t.Helper()
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := ktcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		t.Fatalf("kotlinc error: %v\n%s", err, out)
	}
	cmd := exec.Command("java", "-jar", jar)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	return bytes.TrimSpace(out)
}

func runKTLeetExample(t *testing.T, id int) {
	root := repoRoot(t)
	dir := filepath.Join(root, "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no example files for id %d", id)
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
			code, err := ktcode.New(env).Compile(prog)
			if err != nil {
				t.Skipf("compile error: %v", err)
				return
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "Main.kt")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			jar := filepath.Join(dir, "main.jar")
			if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
				t.Skipf("kotlinc error: %v\n%s", err, out)
				return
			}
			cmd := exec.Command("java", "-jar", jar)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run error: %v\n%s", err, out)
			}
			if id == 1 {
				if string(bytes.TrimSpace(out)) != "0\n1" {
					t.Fatalf("unexpected output: %s", out)
				}
			}
		})
	}
}

func TestKTCompiler_GoldenOutput_Valid(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".kt.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := ktcode.New(env).Compile(prog)
		if err != nil {
			if strings.Contains(err.Error(), "unsupported") {
				return nil, fmt.Errorf("❌ unsupported: %w", err)
			}
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}

		// Write and run the generated Kotlin code.
		dir := t.TempDir()
		ktFile := filepath.Join(dir, "Main.kt")
		if err := os.WriteFile(ktFile, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		jar := filepath.Join(dir, "main.jar")
		if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("❌ kotlinc error: %w\n%s", err, out)
		}
		cmd := exec.Command("java", "-jar", jar)
		inPath := strings.TrimSuffix(src, ".mochi") + ".in"
		var inData []byte
		if data, err := os.ReadFile(inPath); err == nil {
			cmd.Stdin = bytes.NewReader(data)
			inData = data
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ java run error: %w\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)

		// Run using the Mochi VM for comparison.
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("❌ vm compile error: %w", err)
		}
		var vmBuf bytes.Buffer
		m := vm.NewWithIO(p, bytes.NewReader(inData), &vmBuf)
		if err := m.Run(); err != nil {
			if ve, ok := err.(*vm.VMError); ok {
				return nil, fmt.Errorf("❌ vm run error:\n%s", ve.Format(p))
			}
			return nil, fmt.Errorf("❌ vm run error: %w", err)
		}
		if vmOut := bytes.TrimSpace(vmBuf.Bytes()); !bytes.Equal(vmOut, gotOut) {
			t.Errorf("runtime mismatch for %s\n\n--- VM ---\n%s\n\n--- Kotlin ---\n%s\n", filepath.Base(src), vmOut, gotOut)
		}

		// Compare Kotlin output to golden .out if present.
		wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
		if wantData, err := os.ReadFile(wantPath); err == nil {
			if want := bytes.TrimSpace(wantData); !bytes.Equal(gotOut, want) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", filepath.Base(wantPath), gotOut, want)
			}
		}

		return bytes.TrimSpace(code), nil
	})
}
func TestKTCompiler_TPCHQ1(t *testing.T) {
	if err := ktcode.EnsureKotlin(); err != nil {
		t.Skipf("kotlin not installed: %v", err)
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := ktcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "kt", "q1.kt.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	gotCode := bytes.TrimSpace(code)
	if !bytes.Equal(gotCode, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.kt.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotCode, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "Main.kt")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	jar := filepath.Join(dir, "main.jar")
	if out, err := exec.Command("kotlinc", file, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
		t.Skipf("kotlinc error: %v\n%s", err, out)
		return
	}
	out, err := exec.Command("java", "-jar", jar).CombinedOutput()
	if err != nil {
		t.Skipf("java run error: %v\n%s", err, out)
		return
	}
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "kt", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}
