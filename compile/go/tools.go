package gocode

import (
	"bytes"
	"fmt"
	"go/format"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
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

// FormatGo formats the provided Go source. It prefers the external tools
// `goimports` or `gofmt` if installed and falls back to go/format.
func FormatGo(src []byte) []byte {
	tool, err := exec.LookPath("goimports")
	if err != nil {
		tool, err = exec.LookPath("gofmt")
	}
	if err == nil {
		cmd := exec.Command(tool)
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			res := out.Bytes()
			if len(res) == 0 || res[len(res)-1] != '\n' {
				res = append(res, '\n')
			}
			return res
		}
	}
	if out, err := format.Source(src); err == nil {
		src = out
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
