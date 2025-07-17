//go:build slow

package mochix_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/golden"
)

func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func runMochix(t *testing.T, args ...string) ([]byte, error) {
	t.Helper()
	root := repoRoot(t)
	cmd := exec.Command("go", append([]string{"run", "-tags", "slow", "./cmd/mochix"}, args...)...)
	cmd.Dir = root
	cmd.Env = append(os.Environ(), "SOURCE_DATE_EPOCH=0", "MOCHI_HEADER_TIME=2006-01-02T15:04:05Z")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, err
	}
	return bytes.TrimSpace(out), nil
}

func TestBuildGo(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".go.out", func(src string) ([]byte, error) {
		return runMochix(t, "build", "--target", "go", src)
	})
}

func TestBuildTS(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".ts", func(src string) ([]byte, error) {
		return runMochix(t, "build", "--target", "ts", src)
	})
}

func TestBuildPy(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".py.out", func(src string) ([]byte, error) {
		return runMochix(t, "build", "--target", "py", src)
	})
}

func TestBuildXClj(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".clj.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "clj", src)
	})
}

func TestBuildXDart(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".dart", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "dart", src)
	})
}

func TestBuildXC(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".c", func(src string) ([]byte, error) {
		out, err := runMochix(t, "buildx", "--target", "c", src)
		if err != nil {
			return nil, err
		}
		return append([]byte("//go:build ignore\n"), out...), nil
	})
}

func TestBuildXJava(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".java.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "java", src)
	})
}

func TestBuildXCpp(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".cpp.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "cpp", src)
	})
}

func TestBuildXRust(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".rs.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "rust", src)
	})
}

func TestBuildXSwift(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".swift.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "swift", src)
	})
}
func TestBuildXCobol(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".cob.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "cobol", src)
	})
}

func TestBuildXCS(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".cs", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "cs", src)
	})
}

func TestBuildXErlang(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".erl", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "erlang", src)
	})
}

func TestBuildXEx(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".ex.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "ex", src)
	})
}

func TestBuildXFortran(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".f90.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "fortran", src)
	})
}

func TestBuildXFS(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".fs", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "fs", src)
	})
}

func TestBuildXHS(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".hs", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "hs", src)
	})
}

func TestBuildXLua(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".lua", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "lua", src)
	})
}

func TestBuildXOcaml(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".ml.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "ocaml", src)
	})
}

func TestBuildXPascal(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".pas.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "pascal", src)
	})
}

func TestBuildXPHP(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".php", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "php", src)
	})
}

func TestBuildXPL(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".pl", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "pl", src)
	})
}

func TestBuildXRacket(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".rkt.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "racket", src)
	})
}

func TestBuildXRuby(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".rb.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "rb", src)
	})
}

func TestBuildXScala(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".scala.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "scala", src)
	})
}

func TestBuildXScheme(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".scm.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "scheme", src)
	})
}

func TestBuildXSmalltalk(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".smalltalk.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "smalltalk", src)
	})
}

func TestBuildXST(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".st.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "st", src)
	})
}

func TestBuildXZig(t *testing.T) {
	golden.Run(t, "tests/mochix", ".mochi", ".zig.out", func(src string) ([]byte, error) {
		return runMochix(t, "buildx", "--target", "zig", src)
	})
}
