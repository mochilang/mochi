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

func TestRunCobol(t *testing.T) {
	if _, err := exec.LookPath("cobc"); err != nil {
		t.Skip("cobc not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".cobol.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "cobol", src, ".cob")
	})
}

func TestRunCS(t *testing.T) {
	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".cs.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "cs", src, ".cs")
	})
}

func TestRunErlang(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".erlang.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "erlang", src, ".erl")
	})
}

func TestRunEx(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".ex.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "ex", src, ".exs")
	})
}

func TestRunFortran(t *testing.T) {
	if _, err := exec.LookPath("gfortran"); err != nil {
		t.Skip("gfortran not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".fortran.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "fortran", src, ".f90")
	})
}

func TestRunFS(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".fs.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "fs", src, ".fs")
	})
}

func TestRunHS(t *testing.T) {
	if _, err := exec.LookPath("runhaskell"); err != nil {
		t.Skip("runhaskell not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".hs.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "hs", src, ".hs")
	})
}

func TestRunKotlin(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".kotlin.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "kotlin", src, ".kt")
	})
}

func TestRunLua(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".lua.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "lua", src, ".lua")
	})
}

func TestRunOcaml(t *testing.T) {
	if _, err := exec.LookPath("ocaml"); err != nil {
		t.Skip("ocaml not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".ocaml.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "ocaml", src, ".ml")
	})
}

func TestRunPascal(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skip("fpc not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".pascal.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "pascal", src, ".pas")
	})
}

func TestRunPHP(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".php.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "php", src, ".php")
	})
}

func TestRunPL(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".pl.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "pl", src, ".pl")
	})
}

func TestRunRacket(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".racket.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "racket", src, ".rkt")
	})
}

func TestRunRuby(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".rb.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "rb", src, ".rb")
	})
}

func TestRunScala(t *testing.T) {
	if _, err := exec.LookPath("scala"); err != nil {
		t.Skip("scala not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".scala.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "scala", src, ".scala")
	})
}

func TestRunScheme(t *testing.T) {
	if _, err := exec.LookPath("chibi-scheme"); err != nil {
		t.Skip("scheme not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".scheme.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "scheme", src, ".scm")
	})
}

func TestRunSmalltalk(t *testing.T) {
	if _, err := exec.LookPath("gst"); err != nil {
		t.Skip("gst not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".smalltalk.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "smalltalk", src, ".st")
	})
}

func TestRunST(t *testing.T) {
	if _, err := exec.LookPath("gst"); err != nil {
		t.Skip("gst not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".st.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "st", src, ".st")
	})
}

func TestRunZig(t *testing.T) {
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig not installed")
	}
	golden.Run(t, "tests/mochix", ".mochi", ".zig.run", func(src string) ([]byte, error) {
		return buildAndRun(t, "buildx", "zig", src, ".zig")
	})
}
