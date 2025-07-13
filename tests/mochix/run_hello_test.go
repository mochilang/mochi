//go:build slow

package mochix_test

import (
	"bytes"
	"fmt"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
)

func buildAndRun(t *testing.T, subcmd, lang, src, ext string) ([]byte, error) {
	tmpDir := t.TempDir()
	outFile := filepath.Join(tmpDir, "hello"+ext)
	if _, err := runMochix(t, subcmd, "--target", lang, src, "--out", outFile); err != nil {
		return nil, fmt.Errorf("build error: %w", err)
	}
	var cmd *exec.Cmd
	switch lang {
	case "go":
		cmd = exec.Command("go", "run", outFile)
	case "ts":
		jsDir := filepath.Join(tmpDir, "js")
		if out, err := exec.Command("tsc", outFile, "--outDir", jsDir).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("tsc: %w\n%s", err, out)
		}
		jsFile := filepath.Join(jsDir, "hello.js")
		cmd = exec.Command("node", jsFile)
	case "py":
		cmd = exec.Command("python3", outFile)
	case "clj":
		cmd = exec.Command("clojure", outFile)
	case "dart":
		cmd = exec.Command("dart", outFile)
	case "c":
		bin := filepath.Join(tmpDir, "hello_c")
		if out, err := exec.Command("gcc", outFile, "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("gcc: %w\n%s", err, out)
		}
		cmd = exec.Command(bin)
	case "cpp":
		bin := filepath.Join(tmpDir, "hello_cpp")
		if out, err := exec.Command("g++", outFile, "-std=c++17", "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("g++: %w\n%s", err, out)
		}
		cmd = exec.Command(bin)
	case "rust":
		bin := filepath.Join(tmpDir, "hello_rust")
		if out, err := exec.Command("rustc", outFile, "-O", "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("rustc: %w\n%s", err, out)
		}
		cmd = exec.Command(bin)
	case "swift":
		bin := filepath.Join(tmpDir, "hello_swift")
		if out, err := exec.Command("swiftc", outFile, "-o", bin).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("swiftc: %w\n%s", err, out)
		}
		cmd = exec.Command(bin)
	case "java":
		if out, err := exec.Command("javac", outFile).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("javac: %w\n%s", err, out)
		}
		className := strings.TrimSuffix(filepath.Base(outFile), filepath.Ext(outFile))
		cmd = exec.Command("java", "-cp", tmpDir, className)
	default:
		return nil, fmt.Errorf("unsupported language: %s", lang)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return out, fmt.Errorf("run error: %w", err)
	}
	return bytes.TrimSpace(out), nil
}

func TestRunGo(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".go.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "build", "go", src, ".go")
	})
}

func TestRunTS(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".ts.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "build", "ts", src, ".ts")
	})
}

func TestRunPy(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".py.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "build", "py", src, ".py")
	})
}

func TestRunClj(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".clj.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "clj", src, ".clj")
	})
}

func TestRunDart(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".dart.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "dart", src, ".dart")
	})
}

func TestRunC(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".c.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "c", src, ".c")
	})
}

func TestRunCpp(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".cpp.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "cpp", src, ".cpp")
	})
}

func TestRunRust(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".rs.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "rust", src, ".rs")
	})
}

func TestRunSwift(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".swift.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "swift", src, ".swift")
	})
}

func TestRunJava(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".java.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "java", src, ".java")
	})
}
