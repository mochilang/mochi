package rscode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureRust verifies that the Rust toolchain is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureRust() error {
	return ensureRust()
}

// EnsureRustfmt verifies that the rustfmt tool is installed and attempts to
// install it via rustup if missing. It is safe to call from tests.
func EnsureRustfmt() error {
	if _, err := exec.LookPath("rustfmt"); err == nil {
		return nil
	}
	if rustup, err := exec.LookPath("rustup"); err == nil {
		cmd := exec.Command(rustup, "component", "add", "rustfmt")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err == nil {
			return nil
		}
	}
	return fmt.Errorf("rustfmt not found")
}

func ensureRust() error {
	if _, err := exec.LookPath("rustc"); err == nil {
		return nil
	}
	fmt.Println("\U0001F980 Installing Rust...")
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "rustc", "cargo")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "rust")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "rust")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "rust")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	cmd := exec.Command("sh", "-c", "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func Ensure() error {
	if err := EnsureRust(); err != nil {
		return err
	}
	return EnsureRustfmt()
}

// FormatRust runs rustfmt on the given source code if available. If rustfmt is
// missing or fails the formatting step, tabs are expanded to four spaces and a
// trailing newline is ensured so that the generated code remains readable.
func FormatRust(src []byte) []byte {
	if path, err := exec.LookPath("rustfmt"); err == nil {
		cmd := exec.Command(path, "--edition", "2021", "--emit", "stdout")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
