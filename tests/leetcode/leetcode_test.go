//go:build slow

package leetcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
)

func readInput(inPath string) *bytes.Reader {
	data, err := os.ReadFile(inPath)
	if err != nil {
		return nil
	}
	return bytes.NewReader(data)
}

func runStdoutOnly(cmd *exec.Cmd) ([]byte, error) {
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		if stderr.Len() != 0 {
			return nil, fmt.Errorf("%w: %s", err, stderr.String())
		}
		return nil, err
	}
	return bytes.TrimSpace(out), nil
}

func TestCSolutions(t *testing.T) {
	if _, err := exec.LookPath("gcc"); err != nil {
		t.Skip("gcc not installed")
	}
	golden.Run(t, "tests/leetcode/x/c", ".c", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".c") + ".in"
		tmpDir, err := os.MkdirTemp("", "leetcode_c")
		if err != nil {
			return nil, err
		}
		defer os.RemoveAll(tmpDir)
		bin := filepath.Join(tmpDir, "a.out")
		if out, err := exec.Command("gcc", src, "-O2", "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %s", out)
		}
		cmd := exec.Command(bin)
		if data, err := os.ReadFile(inPath); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestCppSolutions(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	golden.Run(t, "tests/leetcode/x/cpp", ".cpp", ".out", func(src string) ([]byte, error) {
		bin := strings.TrimSuffix(src, ".cpp")
		cmd := exec.Command("g++", "-std=c++17", src, "-O2", "-o", bin)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %s", out)
		}
		defer os.Remove(bin)
		run := exec.Command(bin)
		inPath := strings.TrimSuffix(src, ".cpp") + ".in"
		if data, err := os.ReadFile(inPath); err == nil {
			run.Stdin = bytes.NewReader(data)
		}
		out, err := run.CombinedOutput()
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestCsharpSolutions(t *testing.T) {
	if _, err := exec.LookPath("mcs"); err != nil {
		t.Skip("mcs compiler not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono runtime not installed")
	}
	golden.Run(t, "tests/leetcode/x/csharp", ".cs", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".cs") + ".in"
		exePath := strings.TrimSuffix(src, ".cs") + ".exe"
		defer os.Remove(exePath)
		compile := exec.Command("mcs", "-out:"+exePath, src)
		if out, err := compile.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %v: %s", err, out)
		}
		cmd := exec.Command("mono", exePath)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestClojureSolutions(t *testing.T) {
	if _, err := exec.LookPath("bb"); err != nil {
		t.Skip("bb toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/clojure", ".clj", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".clj") + ".in"
		cmd := exec.Command("bb", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestDartSolutions(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/dart", ".dart", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".dart") + ".in"
		cmd := exec.Command("dart", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestElixirSolutions(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/elixir", ".ex", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".ex") + ".in"
		cmd := exec.Command("elixir", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestErlangSolutions(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	golden.Run(t, "tests/leetcode/x/erlang", ".erl", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".erl") + ".in"
		cmd := exec.Command("escript", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestGoSolutions(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/go", ".go", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".go") + ".in"
		cmd := exec.Command("go", "run", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestHaskellSolutions(t *testing.T) {
	if _, err := exec.LookPath("runghc"); err != nil {
		t.Skip("runghc not installed")
	}
	golden.Run(t, "tests/leetcode/x/haskell", ".hs", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".hs") + ".in"
		cmd := exec.Command("runghc", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestJavaSolutions(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	golden.Run(t, "tests/leetcode/x/java", ".java", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".java") + ".in"
		tmpDir := t.TempDir()
		mainPath := filepath.Join(tmpDir, "Main.java")
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		if err := os.WriteFile(mainPath, data, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("javac", "-d", tmpDir, mainPath)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("javac: %v: %s", err, out)
		}
		cmd = exec.Command("java", "-cp", tmpDir, "Main")
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestKotlinSolutions(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc toolchain not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	golden.Run(t, "tests/leetcode/x/kotlin", ".kt", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".kt") + ".in"
		jarPath := filepath.Join(t.TempDir(), "prog.jar")
		cmd := exec.Command("kotlinc", src, "-include-runtime", "-d", jarPath)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile: %v: %s", err, out)
		}
		runCmd := exec.Command("java", "-jar", jarPath)
		runCmd.Stdin = readInput(inPath)
		return runStdoutOnly(runCmd)
	})
}

func TestLeanSolutions(t *testing.T) {
	if _, err := exec.LookPath("lean"); err != nil {
		t.Skip("lean toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/lean", ".lean", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".lean") + ".in"
		cmd := exec.Command("lean", "--run", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestLuaSolutions(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua interpreter not installed")
	}
	golden.Run(t, "tests/leetcode/x/lua", ".lua", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".lua") + ".in"
		cmd := exec.Command("lua", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestMochiSolutions(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".mochi") + ".in"
		cmd := exec.Command("go", "run", "../../cmd/mochi", "run", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestOcamlSolutions(t *testing.T) {
	if _, err := exec.LookPath("ocamlopt"); err != nil {
		t.Skip("ocaml toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/ocaml", ".ml", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(src, ".ml")
		inPath := base + ".in"
		exe := base + ".exe"
		compile := exec.Command("ocamlopt", "-o", exe, src)
		if out, err := compile.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %v: %s", err, out)
		}
		defer os.Remove(exe)
		defer os.Remove(base + ".cmi")
		defer os.Remove(base + ".cmx")
		defer os.Remove(base + ".o")
		cmd := exec.Command(exe)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestPascalSolutions(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skip("fpc compiler not installed")
	}
	golden.Run(t, "tests/leetcode/x/pascal", ".pas", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".pas") + ".in"
		tmpDir, err := os.MkdirTemp("", "leetcode_pascal")
		if err != nil {
			return nil, err
		}
		defer os.RemoveAll(tmpDir)
		exeName := strings.TrimSuffix(filepath.Base(src), ".pas")
		exePath := filepath.Join(tmpDir, exeName)
		cmd := exec.Command("fpc", "-O2", "-v0", "-FE"+tmpDir, "-FU"+tmpDir, "-o"+exePath, src)
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("compile error: %v: %s", err, out)
		}
		run := exec.Command(exePath)
		run.Stdin = readInput(inPath)
		out, err := runStdoutOnly(run)
		if err != nil {
			return nil, fmt.Errorf("run error: %v", err)
		}
		return out, nil
	})
}

func TestPHPSolutions(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	golden.Run(t, "tests/leetcode/x/php", ".php", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".php") + ".in"
		cmd := exec.Command("php", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestPythonSolutions(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	golden.Run(t, "tests/leetcode/x/python", ".py", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".py") + ".in"
		cmd := exec.Command("python3", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestRubySolutions(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	golden.Run(t, "tests/leetcode/x/ruby", ".rb", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".rb") + ".in"
		cmd := exec.Command("ruby", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestRustSolutions(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	golden.Run(t, "tests/leetcode/x/rust", ".rs", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".rs") + ".in"
		tmpDir, err := os.MkdirTemp("", "leetcode_rust")
		if err != nil {
			return nil, err
		}
		defer os.RemoveAll(tmpDir)
		bin := filepath.Join(tmpDir, "prog")
		if out, err := exec.Command("rustc", "-O", src, "-o", bin).CombinedOutput(); err != nil {
			return out, err
		}
		cmd := exec.Command(bin)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestScalaSolutions(t *testing.T) {
	if _, err := exec.LookPath("scala"); err != nil {
		t.Skip("scala toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/scala", ".scala", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".scala") + ".in"
		cmd := exec.Command("scala", "run", "--server=false", "-q", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestSwiftSolutions(t *testing.T) {
	if _, err := exec.LookPath("swift"); err != nil {
		t.Skip("swift toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/swift", ".swift", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".swift") + ".in"
		cmd := exec.Command("swift", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestTypeScriptSolutions(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not installed")
	}
	golden.Run(t, "tests/leetcode/x/typescript", ".ts", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".ts") + ".in"
		cmd := exec.Command("npx", "--yes", "ts-node", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}

func TestZigSolutions(t *testing.T) {
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig toolchain not installed")
	}
	golden.Run(t, "tests/leetcode/x/zig", ".zig", ".out", func(src string) ([]byte, error) {
		inPath := strings.TrimSuffix(src, ".zig") + ".in"
		cmd := exec.Command("zig", "run", src)
		cmd.Stdin = readInput(inPath)
		return runStdoutOnly(cmd)
	})
}
