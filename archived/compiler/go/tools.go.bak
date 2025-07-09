//go:build archived

package gocode

import (
	"bytes"
	"fmt"
	"go/format"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"

	"golang.org/x/tools/imports"
)

// EnsureMochi builds the Mochi command and returns its path. It is used by
// benchmarks to compile and run programs.
func EnsureMochi() (string, error) {
	if path, err := exec.LookPath("mochi"); err == nil {
		return path, nil
	}
	fmt.Println("🔧 Mochi not found, building...")
	home := os.Getenv("HOME")
	if home == "" {
		return "", fmt.Errorf("HOME not set")
	}
	out := filepath.Join(home, "bin", "mochi")
	if runtime.GOOS == "windows" {
		out += ".exe"
	}
	if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil {
		return "", err
	}
	cmd := exec.Command("go", "build", "-o", out, "./cmd/mochi")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	return out, nil
}

// EnsureGo verifies that the Go toolchain is installed. If missing, it attempts
// a best-effort installation using the platform's package manager.
func EnsureGo() error {
	if _, err := exec.LookPath("go"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F527 Installing Go via Homebrew...")
			cmd := exec.Command("brew", "install", "go")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F527 Installing Go...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "golang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("\U0001F527 Installing Go via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "golang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("\U0001F527 Installing Go via Scoop...")
			cmd := exec.Command("scoop", "install", "go")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("go"); err == nil {
		return nil
	}
	return fmt.Errorf("go not found")
}

// EnsureGopls installs gopls if it is not present in PATH.
// It requires the Go toolchain to be available.
func EnsureGopls() error {
	if _, err := exec.LookPath("gopls"); err == nil {
		return nil
	}
	if err := EnsureGo(); err != nil {
		return err
	}
	fmt.Println("\U0001F527 Installing gopls...")
	cmd := exec.Command("go", "install", "golang.org/x/tools/gopls@latest")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	_ = cmd.Run()
	if _, err := exec.LookPath("gopls"); err == nil {
		return nil
	}
	return fmt.Errorf("gopls not found")
}

// FormatGo formats the provided Go source. It first uses the standard library
// formatter and then, if available, attempts to run `goimports` or `gofmt` for
// additional cleanup. The input is returned with a trailing newline.
func FormatGo(src []byte) []byte {
	if out, err := format.Source(src); err == nil {
		src = out
	}
	if path, err := exec.LookPath("goimports"); err == nil {
		cmd := exec.Command(path)
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	} else if out, err := imports.Process("", src, nil); err == nil {
		src = out
	} else if path, err := exec.LookPath("gofmt"); err == nil {
		cmd := exec.Command(path)
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

// EnsureFormatter verifies that either `gofmt` or `goimports` is available so
// generated Go code can be nicely formatted. It does not fail if neither tool is
// present but returns an error for informational purposes.
func EnsureFormatter() error {
	if _, err := exec.LookPath("gofmt"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("goimports"); err == nil {
		return nil
	}
	return fmt.Errorf("gofmt or goimports not found")
}
