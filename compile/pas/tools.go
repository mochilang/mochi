package pascode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureFPC checks for the Free Pascal compiler and attempts to
// install it via apt or Homebrew if missing. It is safe to call from tests.
func EnsureFPC() (string, error) {
	if path, err := exec.LookPath("fpc"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			cmd = exec.Command("apt-get", "install", "-y", "fp-compiler")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "fpc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
		}
	}
	if path, err := exec.LookPath("fpc"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("fpc not installed")
}
