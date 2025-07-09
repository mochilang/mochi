//go:build slow

package pycode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsurePython installs Python3 if missing. Useful for benchmarks and tests.
func EnsurePython() error {
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F40D Installing Python3 via Homebrew...")
			cmd := exec.Command("brew", "install", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F40D Installing Python3...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "python3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🐍 Installing Python via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🐍 Installing Python via Scoop...")
			cmd := exec.Command("scoop", "install", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	return fmt.Errorf("python3 not found")
}

// EnsurePyPy installs pypy3 if missing. Useful for benchmarks.
func EnsurePyPy() error {
	if _, err := exec.LookPath("pypy3"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F40D Installing PyPy via Homebrew...")
			cmd := exec.Command("brew", "install", "pypy3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F40D Installing PyPy3...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "pypy3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🐍 Installing PyPy via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "pypy3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("pypy3"); err == nil {
		return nil
	}
	return fmt.Errorf("pypy3 not found")
}

// EnsureCython installs cython3 if missing. Useful for benchmarks.
func EnsureCython() error {
	if _, err := exec.LookPath("cython3"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F40D Installing Cython via Homebrew...")
			cmd := exec.Command("brew", "install", "cython")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F40D Installing Cython...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "cython3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🐍 Installing Cython via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "cython")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("cython3"); err == nil {
		return nil
	}
	return fmt.Errorf("cython3 not found")
}

// EnsureBlack installs the Black formatter if missing. Useful for codegen tests.
func EnsureBlack() error {
	if _, err := exec.LookPath("black"); err == nil {
		return nil
	}
	// attempt installation via pip
	installers := []string{"pip3", "pip"}
	for _, bin := range installers {
		if _, err := exec.LookPath(bin); err == nil {
			fmt.Println("\U0001F40D Installing Black via", bin, "...")
			cmd := exec.Command(bin, "install", "--user", "black")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			break
		}
	}
	if _, err := exec.LookPath("black"); err == nil {
		return nil
	}
	return fmt.Errorf("black not found")
}

// EnsurePyright installs the Pyright language server if needed. It attempts
// installation using npm or pip.
func EnsurePyright() error {
	if _, err := exec.LookPath("pyright-langserver"); err == nil {
		return nil
	}
	installers := []struct {
		bin  string
		args []string
	}{
		{"npm", []string{"install", "-g", "pyright"}},
		{"pip3", []string{"install", "--user", "pyright"}},
		{"pip", []string{"install", "--user", "pyright"}},
	}
	for _, inst := range installers {
		if _, err := exec.LookPath(inst.bin); err == nil {
			fmt.Println("\U0001F40D Installing Pyright via", inst.bin, "...")
			cmd := exec.Command(inst.bin, inst.args...)
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			break
		}
	}
	if _, err := exec.LookPath("pyright-langserver"); err == nil {
		return nil
	}
	return fmt.Errorf("pyright-langserver not found")
}

// FormatPy formats Python source using `black` if available and
// also passes the code through Python's `ast.unparse` to reduce
// unnecessary parentheses. If neither tool is available, the input
// is returned unchanged.
func FormatPy(src []byte) []byte {
	header := []byte("# Generated by Mochi Python compiler\n")
	hasHeader := bytes.HasPrefix(src, header)
	if hasHeader {
		src = src[len(header):]
	}

	// First try to canonicalise the code via the Python AST.
	if python, err := exec.LookPath("python3"); err == nil {
		script := "import ast,sys;\n" +
			"src=sys.stdin.read();\n" +
			"try:\n" +
			" tree=ast.parse(src);\n" +
			" src=ast.unparse(tree)\n" +
			"except Exception:\n" +
			" pass;\n" +
			"sys.stdout.write(src)"
		cmd := exec.Command(python, "-c", script)
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	}

	// Then run Black for formatting if it exists.
	if tool, err := exec.LookPath("black"); err == nil {
		cmd := exec.Command(tool, "-q", "-")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	}

	if hasHeader {
		src = append(header, src...)
	}

	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
